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
