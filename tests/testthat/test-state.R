# ---- I4 (#48) : etat partage entre processus ------------------------------------

test_that("two report calls in one session are spaced by min_interval", {
  local_fresh_state()
  waited <- c()
  local_mocked_bindings(toggl_sleep = function(s) waited <<- c(waited, s))
  toggl_pace(10)
  toggl_pace(10)
  expect_length(waited, 1)
  expect_gt(waited, 9)
  expect_lte(waited, 10)
})

test_that("without a state file, another process does not see the pacing (documented limit)", {
  local_fresh_state()
  waited <- c()
  local_mocked_bindings(toggl_sleep = function(s) waited <<- c(waited, s))
  toggl_pace(10)
  forget_session()
  toggl_pace(10)
  expect_length(waited, 0)
})

test_that("with togglr.state_file, the pacing is shared between processes (#48)", {
  local_fresh_state()
  withr::local_options(togglr.state_file = withr::local_tempfile())
  waited <- c()
  local_mocked_bindings(toggl_sleep = function(s) waited <<- c(waited, s))
  toggl_pace(10)
  forget_session()
  toggl_pace(10)
  expect_length(waited, 1)
  expect_gt(waited, 9)
})

test_that("with togglr.state_file, concurrent callers each reserve their own slot (#48)", {
  local_fresh_state()
  withr::local_options(togglr.state_file = withr::local_tempfile())
  waited <- c()
  local_mocked_bindings(toggl_sleep = function(s) waited <<- c(waited, s))
  # trois workers demandent un creneau avant que le premier n'ait dormi
  for (i in 1:3) { forget_session(); toggl_pace(10) }
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
  expect_no_error(toggl_pace(10))
})

# ---- M6 (#64) : espacement applique a toutes les requetes ------------------------

test_that("every request is spaced by togglr.min_interval, not only the detailled pages (#64)", {
  local_fresh_state()
  withr::local_options(togglr.min_interval = 10)
  waited <- c()
  srv <- serve(fake_response(200, list(a = 1)), fake_response(200, list(a = 2)))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) waited <<- c(waited, s))
  toggl_GET("https://api.track.toggl.com/api/v9/me", api_token = "t")
  toggl_GET("https://api.track.toggl.com/reports/api/v2/summary", api_token = "t")
  expect_length(waited, 1)
  expect_gt(waited, 9)
})

test_that("a retry is spaced too (#64)", {
  local_fresh_state()
  withr::local_options(togglr.min_interval = 10)
  waited <- c()
  srv <- serve(fake_response(502), fake_response(200, list(ok = TRUE)))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) waited <<- c(waited, s))
  suppressMessages(toggl_GET("u", api_token = "t"))
  expect_equal(waited[1], 2)    # backoff du 5xx
  expect_gt(max(waited[-1]), 7) # puis le creneau suivant (10 s apres le premier envoi)
})

test_that("writes are spaced too (#64)", {
  local_fresh_state()
  withr::local_options(togglr.min_interval = 10)
  waited <- c()
  srv <- serve_write(fake_response(200, list(id = 1L)), fake_response(200, list(id = 2L)))
  local_mocked_bindings(POST = srv$send, toggl_sleep = function(s) waited <<- c(waited, s))
  toggl_request("POST", "u", api_token = "t", body = "{}")
  toggl_request("POST", "u", api_token = "t", body = "{}")
  expect_length(waited, 1)
  expect_gt(waited, 9)
})
