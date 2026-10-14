# ---- T1 (#73) : cas limites ------------------------------------------------------------

test_that("a totals answer without seconds is an error, not an empty period (#73, #76)", {
  local_fresh_state()
  local_mocked_bindings(POST = function(url, ...) fake_response(200, list(rates = list())),
                        toggl_sleep = function(s) NULL)
  expect_error(suppressMessages(report()), "no total duration")
})

test_that("a 402 on the last attempt raises the API error (#73)", {
  local_fresh_state()
  srv <- serve(fake_response(402, list(error = "quota"), headers = list(`x-toggl-quota-resets-in` = "10")))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  expect_error(toggl_GET("u", api_token = "t", max_tries = 1), "toggl API error 402")
})

test_that("an expired quota is forgotten and the request is sent (#73)", {
  local_fresh_state()
  toggl_state_set(toggl_quota_key("t"), Sys.time() - 5)
  srv <- serve(fake_response(200, list(ok = TRUE)))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) stop("should not wait"))
  expect_true(toggl_GET("u", api_token = "t")$content$ok)
  expect_null(toggl_state_get(toggl_quota_key("t")))
})

test_that("a retry-after longer than max_wait fails at once, without waiting (#73)", {
  local_fresh_state()
  srv <- serve(fake_response(429, headers = list(`retry-after` = "600")))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) stop("should not wait"))
  expect_error(toggl_GET("u", api_token = "t", max_wait = 60), "asks to wait 600s")
  expect_equal(srv$calls(), 1)
})

test_that("cache = FALSE writes nothing on disk (#73)", {
  local_fresh_state()
  dir <- withr::local_tempdir()
  withr::local_options(togglr.cache_dir = dir)
  srv <- serve_v3(list(v3_page(1:3)), totals = 3 * 3600)
  local_mocked_bindings(POST = srv$send, toggl_sleep = function(s) NULL)
  suppressMessages(report())
  expect_length(list.files(dir, recursive = TRUE), 0)
})

test_that("the error message never shows the query string of the url (#73)", {
  local_fresh_state()
  srv <- serve(fake_response(403, "forbidden"))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  err <- tryCatch(toggl_GET("https://x/api?workspace_id=1&secret=abc", api_token = "t"), error = conditionMessage)
  expect_false(grepl("secret=abc", err, fixed = TRUE))
})

test_that("get_workspace_id fails clearly when the account has no workspace (#73)", {
  local_fresh_state()
  srv <- serve(fake_response(200, list()))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  expect_error(get_workspace_id(api_token = "t"), "cant find workspace id")
})

test_that("get_current returns NULL when nothing is running (#73)", {
  local_fresh_state()
  srv <- serve(fake_response(200, NULL))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  expect_null(get_current(api_token = "t"))
})
