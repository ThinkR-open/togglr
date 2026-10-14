# ---- I4 (#48) : etat partage entre processus ------------------------------------

test_that("two report calls in one session are spaced by min_interval", {
  local_fresh_state()
  waited <- c()
  local_mocked_bindings(toggl_sleep = function(s) waited <<- c(waited, s))
  toggl_reports_pace(10)
  toggl_reports_pace(10)
  expect_length(waited, 1)
  expect_gt(waited, 9)
  expect_lte(waited, 10)
})

test_that("without a state file, another process does not see the pacing (documented limit)", {
  local_fresh_state()
  waited <- c()
  local_mocked_bindings(toggl_sleep = function(s) waited <<- c(waited, s))
  toggl_reports_pace(10)
  forget_session()
  toggl_reports_pace(10)
  expect_length(waited, 0)
})

test_that("with togglr.state_file, the pacing is shared between processes (#48)", {
  local_fresh_state()
  withr::local_options(togglr.state_file = withr::local_tempfile())
  waited <- c()
  local_mocked_bindings(toggl_sleep = function(s) waited <<- c(waited, s))
  toggl_reports_pace(10)
  forget_session()
  toggl_reports_pace(10)
  expect_length(waited, 1)
  expect_gt(waited, 9)
})

test_that("with togglr.state_file, concurrent callers each reserve their own slot (#48)", {
  local_fresh_state()
  withr::local_options(togglr.state_file = withr::local_tempfile())
  waited <- c()
  local_mocked_bindings(toggl_sleep = function(s) waited <<- c(waited, s))
  # trois workers demandent un creneau avant que le premier n'ait dormi
  for (i in 1:3) { forget_session(); toggl_reports_pace(10) }
  expect_length(waited, 2)
  expect_equal(round(waited), c(10, 20))
})

test_that("with togglr.state_file, an exhausted quota is known by every process (#48)", {
  local_fresh_state()
  withr::local_options(togglr.state_file = withr::local_tempfile())
  srv <- serve(fake_response(402, headers = list(`x-toggl-quota-remaining` = "0",
                                                 `x-toggl-quota-resets-in` = "1800")))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  expect_error(toggl_GET("u", api_token = "t", max_wait = 60), "quota")
  forget_session()
  expect_error(toggl_GET("u", api_token = "t", max_wait = 60), "no request sent")
  expect_equal(srv$calls(), 1)
})

test_that("an unreadable state file is not fatal: it is started again", {
  local_fresh_state()
  f <- withr::local_tempfile()
  writeLines("not an rds", f)
  withr::local_options(togglr.state_file = f)
  local_mocked_bindings(toggl_sleep = function(s) NULL)
  expect_no_error(toggl_reports_pace(10))
})
