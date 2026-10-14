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

# ---- I1 (#45) : timeout et erreurs reseau ------------------------------------

test_that("every GET carries a timeout, 120 s by default, settable by option (#45)", {
  seen <- NULL
  local_mocked_bindings(GET = function(...) { seen <<- list(...); fake_response(200) })
  toggl_http_get("u", "t")
  conf <- Filter(function(a) inherits(a, "request"), seen)
  timeouts <- unname(unlist(lapply(conf, function(r) r$options$timeout_ms)))
  expect_equal(timeouts, 120000)

  withr::local_options(togglr.timeout = 5)
  toggl_http_get("u", "t")
  conf <- Filter(function(a) inherits(a, "request"), seen)
  expect_equal(unname(unlist(lapply(conf, function(r) r$options$timeout_ms))), 5000)
})

test_that("a network error is retried like a 5xx (#45)", {
  calls <- 0
  waited <- c()
  local_mocked_bindings(
    toggl_http_get = function(url, api_token) {
      calls <<- calls + 1
      if (calls == 1) stop("Timeout was reached: Operation timed out after 120000 milliseconds")
      fake_response(200, list(ok = TRUE))
    },
    toggl_sleep = function(s) waited <<- c(waited, s)
  )
  expect_message(res <- toggl_GET("u", api_token = "t"), "network error")
  expect_true(res$content$ok)
  expect_equal(calls, 2)
  expect_equal(waited, 2)
})

test_that("a network error that persists fails after max_tries, with the cause (#45)", {
  calls <- 0
  local_mocked_bindings(
    toggl_http_get = function(url, api_token) { calls <<- calls + 1; stop("Could not resolve host") },
    toggl_sleep = function(s) NULL
  )
  expect_error(suppressMessages(toggl_GET("https://x/api?token=secret", api_token = "t", max_tries = 3)),
               "network error.*Could not resolve host")
  expect_equal(calls, 3)
})
