test_that("a 200 answer is returned parsed", {
  srv <- serve(fake_response(200, list(a = 1)))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  expect_equal(toggl_GET("u", api_token = "t")$content, list(a = 1))
})

test_that("a 4xx other than 429 stops at once", {
  srv <- serve(fake_response(403, list(error = "forbidden")))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  expect_error(toggl_GET("u", api_token = "t"), "toggl API error 403")
  expect_equal(srv$calls(), 1)
})

test_that("a rate-limit 429 is retried after the asked delay", {
  waited <- c()
  srv <- serve(
    fake_response(429, headers = list(`retry-after` = "2", `x-toggl-quota-remaining` = "100",
                                      `x-toggl-quota-resets-in` = "3000")),
    fake_response(200, list(ok = TRUE))
  )
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) waited <<- c(waited, s))
  expect_true(toggl_GET("u", api_token = "t")$content$ok)
  expect_equal(waited, 2)
})

test_that("a 402 (hourly quota) fails fast and no request is sent before the reset", {
  .togglr_env$quota_until <- NULL
  withr::defer(.togglr_env$quota_until <- NULL)
  srv <- serve(fake_response(402, headers = list(`x-toggl-quota-remaining` = "0",
                                                 `x-toggl-quota-resets-in` = "1800")))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  expect_error(toggl_GET("u", api_token = "t"), "quota exhausted")
  expect_error(toggl_GET("u2", api_token = "t"), "no request sent")
  expect_equal(srv$calls(), 1)
})

test_that("a 402 waits for the quota reset when max_wait allows it", {
  .togglr_env$quota_until <- NULL
  withr::defer(.togglr_env$quota_until <- NULL)
  waited <- c()
  srv <- serve(fake_response(402, headers = list(`x-toggl-quota-resets-in` = "1800")),
               fake_response(200, list(ok = TRUE)))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) waited <<- c(waited, s))
  expect_true(toggl_GET("u", api_token = "t", max_wait = 3600)$content$ok)
  expect_true(waited[1] >= 1795 && waited[1] <= 1800)
})

test_that("a 429 without retry-after backs off for a minute, not seconds", {
  waited <- c()
  srv <- serve(fake_response(429), fake_response(200, list(ok = TRUE)))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) waited <<- c(waited, s))
  expect_true(toggl_GET("u", api_token = "t")$content$ok)
  expect_equal(waited, 60)
})

test_that("5xx are retried, then fail when the budget is exhausted", {
  srv <- serve(fake_response(502), fake_response(502), fake_response(502))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  expect_error(toggl_GET("u", api_token = "t", max_tries = 3), "toggl API error 502")
  expect_equal(srv$calls(), 3)
})
