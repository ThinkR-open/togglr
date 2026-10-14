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
  local_fresh_state()
  srv <- serve(fake_response(402, headers = list(`x-toggl-quota-remaining` = "0",
                                                 `x-toggl-quota-resets-in` = "1800")))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  expect_error(toggl_GET("u", api_token = "t"), "quota exhausted")
  expect_error(toggl_GET("u2", api_token = "t"), "no request sent")
  expect_equal(srv$calls(), 1)
})

test_that("a 402 waits for the quota reset when max_wait allows it", {
  local_fresh_state()
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

# ---- I5 (#49) : 402 sans quota, quota par token ---------------------------------

test_that("a 402 without quota headers (paid feature) stops at once and blocks nothing (#49)", {
  local_fresh_state()
  srv <- serve(fake_response(402, list(error = "This feature requires a Premium plan")),
               fake_response(200, list(ok = TRUE)))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  expect_error(toggl_GET("u", api_token = "t"), "402.*Premium plan")
  expect_equal(srv$calls(), 1)
  expect_true(toggl_GET("u2", api_token = "t")$content$ok) # pas de blocage d'une heure
})

test_that("a 402 with remaining = 0 but no reset delay is a quota, remembered one hour (#49)", {
  local_fresh_state()
  srv <- serve(fake_response(402, headers = list(`x-toggl-quota-remaining` = "0")))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  expect_error(toggl_GET("u", api_token = "t"), "quota")
  expect_error(toggl_GET("u2", api_token = "t"), "resets in 3[56][0-9]{2}s.*no request sent")
  expect_equal(srv$calls(), 1)
})

test_that("an exhausted quota only blocks the token that received it (#49)", {
  local_fresh_state()
  srv <- serve(fake_response(402, headers = list(`x-toggl-quota-resets-in` = "1800")),
               fake_response(200, list(ok = TRUE)))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  expect_error(toggl_GET("u", api_token = "token_a"), "quota")
  expect_true(toggl_GET("u", api_token = "token_b")$content$ok)
  expect_error(toggl_GET("u", api_token = "token_a"), "no request sent")
})

test_that("the state file never contains the token (#49)", {
  local_fresh_state()
  f <- withr::local_tempfile()
  withr::local_options(togglr.state_file = f)
  srv <- serve(fake_response(402, headers = list(`x-toggl-quota-resets-in` = "1800")))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  expect_error(toggl_GET("u", api_token = "my_secret_token_123"), "quota")
  st <- readRDS(f)
  expect_false(any(grepl("my_secret_token_123", c(names(unlist(st)), as.character(unlist(st))))))
  con <- gzfile(f, "rb")
  brut <- readBin(con, "raw", 1e6)
  close(con)
  expect_false(grepl("my_secret_token_123", rawToChar(brut[brut != as.raw(0)]), fixed = TRUE, useBytes = TRUE))
})

# ---- M10 (#68) : x-toggl-quota-remaining exploite -------------------------------------

quota_ok <- function(remaining, resets = 1800, body = list(ok = TRUE)) {
  fake_response(200, body, headers = list(`x-toggl-quota-remaining` = as.character(remaining),
                                          `x-toggl-quota-resets-in` = as.character(resets)))
}

test_that("a 200 announcing 0 remaining requests: the next one is not sent before the reset (#68)", {
  local_fresh_state()
  srv <- serve(quota_ok(0), quota_ok(239))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  expect_true(toggl_GET("u", api_token = "t")$content$ok)
  expect_error(toggl_GET("u", api_token = "t", max_wait = 60), "no request sent")
  expect_equal(srv$calls(), 1)
})

test_that("few remaining requests are spread until the reset (#68)", {
  local_fresh_state()
  withr::local_options(togglr.min_interval = 0)
  waited <- c()
  srv <- serve(quota_ok(4, resets = 400), quota_ok(3, resets = 300))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) waited <<- c(waited, s))
  toggl_GET("u", api_token = "t")
  expect_message(toggl_GET("u", api_token = "t", max_wait = 3600), "almost exhausted")
  expect_length(waited, 1)
  expect_gt(waited, 95)
  expect_lte(waited, 100)
})

# ---- revue PR #75 ------------------------------------------------------------------

test_that("the spreading of the quota never waits more than max_wait, and says so (revue #75)", {
  local_fresh_state()
  withr::local_options(togglr.min_interval = 0)
  waited <- c()
  srv <- serve(quota_ok(2, resets = 3000), quota_ok(1, resets = 2900))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) waited <<- c(waited, s))
  toggl_GET("u", api_token = "t")
  expect_message(toggl_GET("u", api_token = "t", max_wait = 60), "waiting 60s")
  expect_equal(waited, 60)
})

test_that("a token close to its quota does not slow down another token (revue #75)", {
  local_fresh_state()
  withr::local_options(togglr.min_interval = 0)
  waited <- c()
  srv <- serve(quota_ok(3, resets = 1800), quota_ok(200))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) waited <<- c(waited, s))
  toggl_GET("u", api_token = "token_a")
  toggl_GET("u", api_token = "token_b")
  expect_length(waited, 0)
})

test_that("an exhausted workspace does not block /me nor another workspace (revue #75)", {
  local_fresh_state()
  srv <- serve(fake_response(402, headers = list(`x-toggl-quota-resets-in` = "1800")),
               fake_response(200, list(ok = TRUE)), fake_response(200, list(ok = TRUE)))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  ws1 <- "https://api.track.toggl.com/api/v9/workspaces/1/projects"
  expect_error(toggl_GET(ws1, api_token = "t", max_tries = 1), "402")
  expect_true(toggl_GET("https://api.track.toggl.com/api/v9/me", api_token = "t")$content$ok)
  expect_true(toggl_GET("https://api.track.toggl.com/reports/api/v2/details?workspace_id=2",
                        api_token = "t")$content$ok)
  expect_error(toggl_GET("https://api.track.toggl.com/reports/api/v2/details?workspace_id=1&page=2",
                         api_token = "t"), "no request sent")
  expect_equal(srv$calls(), 3)
})

test_that("the quota scope comes from the url (revue #75)", {
  expect_equal(toggl_quota_scope("https://api.track.toggl.com/api/v9/me"), "user")
  expect_equal(toggl_quota_scope("https://api.track.toggl.com/api/v9/workspaces"), "user")
  expect_equal(toggl_quota_scope("https://api.track.toggl.com/api/v9/workspaces/12/clients"), "ws12")
  expect_equal(toggl_quota_scope("https://api.track.toggl.com/reports/api/v2/details?workspace_id=12&page=1"), "ws12")
  expect_equal(toggl_quota_scope("https://api.track.toggl.com/api/v9/organizations/3/users"), "org3")
})

test_that("a processed POST is returned even if the quota cannot be recorded (revue #75)", {
  local_fresh_state()
  srv <- serve_write(quota_ok(3, body = list(id = 7L)))
  local_mocked_bindings(POST = srv$send, toggl_sleep = function(s) NULL,
                        toggl_note_quota = function(...) stop("could not lock the togglr state file"))
  expect_warning(res <- toggl_request("POST", "u", api_token = "t", body = "{}"), "request succeeded")
  expect_equal(res$content$id, 7L)
})

test_that("one lock pass per request, and a read does not rewrite the state file (revue #75)", {
  local_fresh_state()
  f <- withr::local_tempfile()
  withr::local_options(togglr.state_file = f)
  toggl_state_set("x", 1)
  before <- file.info(f)$mtime
  Sys.sleep(1.1)
  expect_equal(toggl_state_get("x"), 1)
  expect_equal(file.info(f)$mtime, before)

  passes <- 0
  real <- toggl_state_update
  srv <- serve(fake_response(200, list(ok = TRUE)))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL,
                        toggl_state_update = function(f) { passes <<- passes + 1; real(f) })
  toggl_GET("u", api_token = "t")
  expect_equal(passes, 1)
})

test_that("plenty of remaining requests: no slowdown (#68)", {
  local_fresh_state()
  waited <- c()
  srv <- serve(quota_ok(200), quota_ok(199))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) waited <<- c(waited, s))
  toggl_GET("u", api_token = "t")
  toggl_GET("u", api_token = "t")
  expect_length(waited, 0)
})
