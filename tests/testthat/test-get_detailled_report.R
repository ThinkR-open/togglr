report <- function(...) {
  get_detailled_report(api_token = "t", workspace_id = 1,
                       since = as.Date("2026-10-01"), until = as.Date("2026-10-31"), ...)
}

test_that("all the pages announced by total_count are fetched", {
  srv <- serve(
    fake_response(200, report_page(1:50, total = 120)),
    fake_response(200, report_page(51:100, total = 120)),
    fake_response(200, report_page(101:120, total = 120))
  )
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  out <- report()
  expect_equal(nrow(out), 120)
  expect_equal(srv$calls(), 3)
})

test_that("an HTTP error in the middle of a period is an error, not a truncation", {
  srv <- serve(
    fake_response(200, report_page(1:50, total = 120)),
    fake_response(403, list(error = "forbidden"))
  )
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  expect_error(report(), "403")
})

test_that("entries moving during pagination trigger a second full fetch", {
  srv <- serve(
    fake_response(200, report_page(1:50, total = 60)),
    fake_response(200, report_page(50:60, total = 61)),   # a new entry arrived
    fake_response(200, report_page(1:50, total = 61)),
    fake_response(200, report_page(51:61, total = 61))
  )
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  out <- report()
  expect_equal(nrow(out), 61)
  expect_false(anyDuplicated(out$id) > 0)
})

test_that("a period still incomplete after the retry raises an error", {
  short <- fake_response(200, report_page(1:40, total = 45))
  srv <- serve(short, short)
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  expect_error(report(), "incomplete toggl detailled report")
})

test_that("an empty period returns the empty structure", {
  srv <- serve(fake_response(200, report_page(integer(0), total = 0)))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  out <- report()
  expect_equal(nrow(out), 0)
  expect_true("tags" %in% names(out))
})

test_that("cache = TRUE stores only complete periods and reuses them", {
  dir <- withr::local_tempdir()
  srv <- serve(fake_response(200, report_page(1:3, total = 4)),
               fake_response(200, report_page(1:3, total = 4)))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  expect_error(report(cache = TRUE, memoise_cache_dir = dir))
  expect_length(list.files(dir), 0)

  srv <- serve(fake_response(200, report_page(1:4, total = 4)))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  first <- report(cache = TRUE, memoise_cache_dir = dir)
  again <- report(cache = TRUE, memoise_cache_dir = dir)
  expect_equal(srv$calls(), 1)
  expect_equal(first, again)
})

# ---- I2 (#46) : types de colonnes stables -----------------------------------

with_task <- function(page, task = "a task", tid = 99L) {
  page$data <- lapply(page$data, function(e) c(e, list(task = task, tid = tid)))
  page
}

test_that("an empty period has the same column types as a filled one (#46)", {
  e <- empty_detailled_report()
  expect_type(e$task, "character")
  expect_type(e$tid, "integer")
  expect_type(e$id, "double")
  expect_type(e$dur, "double")
})

test_that("a period over a year with an empty year can be combined (#46)", {
  srv <- serve(
    fake_response(200, with_task(report_page(1:3, total = 3))),  # 2016 (fetched first)
    fake_response(200, report_page(integer(0), total = 0))      # 2015 : vide
  )
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  out <- suppressMessages(get_detailled_report(api_token = "t", workspace_id = 1,
                                               since = as.Date("2015-06-01"),
                                               until = as.Date("2016-12-31")))
  expect_equal(nrow(out), 3)
  expect_equal(out$task, rep("a task", 3))
})

test_that("pages with and without tasks are combined with stable types (#46)", {
  srv <- serve(
    fake_response(200, report_page(1:50, total = 53)),              # aucune tache
    fake_response(200, with_task(report_page(51:53, total = 53)))
  )
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  out <- suppressMessages(report())
  expect_type(out$task, "character")
  expect_type(out$tid, "integer")
  expect_equal(sum(!is.na(out$task)), 3)
})

# ---- I3 (#47) : mouvement a total_count constant ------------------------------

test_that("a deletion + an insertion during pagination (same total_count) triggers a new fetch (#47)", {
  srv <- serve(
    fake_response(200, report_page(1:50, total = 60, total_grand = 216000000)),
    # entre les deux pages, l'entree 3 est supprimee et l'entree 61 inseree :
    # meme total_count, mais la somme des durees a change
    fake_response(200, report_page(52:61, total = 60, total_grand = 215000000)),
    fake_response(200, report_page(c(1:2, 4:51), total = 60, total_grand = 215000000)),
    fake_response(200, report_page(52:61, total = 60, total_grand = 215000000))
  )
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  out <- suppressMessages(report())
  expect_equal(srv$calls(), 4)
  expect_setequal(out$id, c(1:2, 4:61))
})

test_that("without total_grand in the answer, total_count alone is still checked (#47)", {
  srv <- serve(
    fake_response(200, report_page(1:50, total = 60)),
    fake_response(200, report_page(51:60, total = 60))
  )
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  expect_equal(nrow(suppressMessages(report())), 60)
  expect_equal(srv$calls(), 2)
})
