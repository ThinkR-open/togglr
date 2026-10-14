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

test_that("a period still incomplete after the retries raises an error", {
  short <- fake_response(200, report_page(1:40, total = 45))
  srv <- serve(short, short, short)
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
  short <- fake_response(200, report_page(1:3, total = 4))
  srv <- serve(short, short, short)
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  expect_error(report(cache = TRUE, cache_dir = dir))
  expect_length(list.files(dir), 0)

  srv <- serve(fake_response(200, report_page(1:4, total = 4)))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  first <- report(cache = TRUE, cache_dir = dir)
  again <- report(cache = TRUE, cache_dir = dir)
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

# ---- I14 (#58) : decoupage au-dela d'un an ----------------------------------------


test_that("367 days (bounds included) are split at the new year (#58)", {
  asked <- local_periods()
  suppressMessages(get_detailled_report(api_token = "t", workspace_id = 1,
                                        since = as.Date("2016-01-01"), until = as.Date("2017-01-01")))
  expect_equal(asked(), c("2016-01-01 2016-12-31", "2017-01-01 2017-01-01"))
})

test_that("a full calendar year, even a leap one, is fetched in one go (#58)", {
  asked <- local_periods()
  suppressMessages(get_detailled_report(api_token = "t", workspace_id = 1,
                                        since = as.Date("2016-01-01"), until = as.Date("2016-12-31")))
  expect_equal(asked(), "2016-01-01 2016-12-31")
})

test_that("less than a year across a new year is fetched in one go (#58)", {
  asked <- local_periods()
  suppressMessages(get_detailled_report(api_token = "t", workspace_id = 1,
                                        since = as.Date("2016-03-01"), until = as.Date("2017-02-28")))
  expect_equal(asked(), "2016-03-01 2017-02-28")
})

test_that("several years give one chunk per calendar year, none longer than a year (#58)", {
  asked <- local_periods()
  out <- suppressMessages(get_detailled_report(api_token = "t", workspace_id = 1,
                                               since = as.Date("2015-06-01"), until = as.Date("2017-12-31")))
  expect_equal(asked(), c("2015-06-01 2015-12-31", "2016-01-01 2016-12-31", "2017-01-01 2017-12-31"))
  expect_equal(nrow(out), 3)
})

# ---- M2 (#60) : max_page deprecie -------------------------------------------------

test_that("max_page warns that it is ignored, and is ignored (#60)", {
  local_fresh_state()
  srv <- serve(fake_response(200, report_page(1:50, total = 60)),
               fake_response(200, report_page(51:60, total = 60)))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  expect_warning(out <- suppressMessages(report(max_page = 1)), "max_page.*deprecated")
  expect_equal(nrow(out), 60)
})

test_that("no warning without max_page (#60)", {
  local_fresh_state()
  srv <- serve(fake_response(200, report_page(1:3, total = 3)))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  expect_no_warning(suppressMessages(report()))
})

# ---- M7 (#65) : nombre de lectures parametrable ------------------------------------

moving_then_stable <- function(n_moves) {
  pages <- list()
  for (k in seq_len(n_moves)) {
    pages <- c(pages, list(fake_response(200, report_page(1:50, total = 60 + k - 1)),
                           fake_response(200, report_page(51:60, total = 60 + k))))
  }
  total <- 60 + n_moves
  c(pages, list(fake_response(200, report_page(1:50, total = total)),
                fake_response(200, report_page(51:total, total = total))))
}

test_that("by default a period is read up to 3 times when it keeps moving (#65)", {
  local_fresh_state()
  srv <- do.call(serve, moving_then_stable(2))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  expect_equal(nrow(suppressMessages(report())), 62)
  expect_equal(srv$calls(), 6)
})

test_that("togglr.report_attempts sets the number of reads (#65)", {
  local_fresh_state()
  withr::local_options(togglr.report_attempts = 1)
  srv <- do.call(serve, moving_then_stable(1))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  expect_error(suppressMessages(report()), "incomplete")
  expect_equal(srv$calls(), 2)
})

# ---- M8 (#66) : le doublon garde la version la plus recente -------------------------

test_that("an entry seen twice keeps its most recently updated version (#66)", {
  local_fresh_state()
  p2 <- report_page(50:60, total = 60)
  p2$data[[1]]$description <- "edited"
  p2$data[[1]]$updated <- "2026-10-12T09:00:00Z" # 11:00 a Paris : plus recent que 10:00+02:00

  srv <- serve(fake_response(200, report_page(1:50, total = 60)), fake_response(200, p2))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  out <- suppressMessages(report())
  expect_equal(nrow(out), 60)
  expect_equal(out$description[out$id == 50], "edited")
})

# ---- M9 (#67) : cache dedie, noms sans memoise --------------------------------------

test_that("the default cache folder is a dedicated sub-folder, settable by option (#67)", {
  withr::local_options(togglr.cache_dir = NULL)
  expect_equal(toggl_cache_dir(), file.path(rappdirs::user_cache_dir("togglr"), "reports"))
  dir <- withr::local_tempdir()
  withr::local_options(togglr.cache_dir = dir)
  local_fresh_state()
  srv <- serve(fake_response(200, report_page(1:4, total = 4)))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  suppressMessages(report(cache = TRUE))
  expect_gt(length(list.files(dir)), 0)
})

test_that("memoise_cache_dir still works, with a deprecation warning (#67)", {
  dir <- withr::local_tempdir()
  local_fresh_state()
  srv <- serve(fake_response(200, report_page(1:4, total = 4)))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  expect_warning(suppressMessages(report(cache = TRUE, memoise_cache_dir = dir)), "memoise_cache_dir.*deprecated")
  expect_gt(length(list.files(dir)), 0)
})

test_that("clean_cache() empties the reports cache; clean_memoise_cache() is deprecated (#67)", {
  dir <- withr::local_tempdir()
  writeLines("x", file.path(dir, "a"))
  clean_cache(cache_dir = dir)
  expect_false(dir.exists(dir))
  old <- withr::local_tempdir()
  expect_warning(clean_memoise_cache(memoise_cache_dir = old), "deprecated.*clean_cache")
})

# ---- revue PR #75 ------------------------------------------------------------------

test_that("the cache is kept per token (revue #75)", {
  local_fresh_state()
  dir <- withr::local_tempdir()
  srv <- serve(fake_response(200, report_page(1:4, total = 4)),
               fake_response(200, report_page(1:2, total = 2)))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  admin <- suppressMessages(get_detailled_report(api_token = "admin", workspace_id = 1,
                                                 since = as.Date("2026-09-01"), until = as.Date("2026-09-30"),
                                                 cache = TRUE, cache_dir = dir))
  member <- suppressMessages(get_detailled_report(api_token = "member", workspace_id = 1,
                                                  since = as.Date("2026-09-01"), until = as.Date("2026-09-30"),
                                                  cache = TRUE, cache_dir = dir))
  expect_equal(srv$calls(), 2)
  expect_equal(nrow(admin), 4)
  expect_equal(nrow(member), 2)
})

test_that("a running timer growing total_grand between pages is not an edit (revue #75)", {
  local_fresh_state()
  local_mocked_bindings(toggl_today = function() as.Date("2026-10-14"))
  srv <- serve(
    fake_response(200, report_page(1:50, total = 60, total_grand = 216000000)),
    fake_response(200, report_page(51:60, total = 60, total_grand = 216000400))
  )
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  expect_equal(nrow(suppressMessages(report())), 60)
  expect_equal(srv$calls(), 2)
})

test_that("on a closed period, any change of total_grand is still an edit (revue #75)", {
  local_fresh_state()
  local_mocked_bindings(toggl_today = function() as.Date("2026-11-14"))
  srv <- serve(
    fake_response(200, report_page(1:50, total = 60, total_grand = 216000000)),
    fake_response(200, report_page(51:60, total = 60, total_grand = 216000400)),
    fake_response(200, report_page(1:50, total = 60, total_grand = 216000400)),
    fake_response(200, report_page(51:60, total = 60, total_grand = 216000400))
  )
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  expect_equal(nrow(suppressMessages(report())), 60)
  expect_equal(srv$calls(), 4)
})
