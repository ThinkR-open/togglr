# ---- T1 (#73) : cas limites ------------------------------------------------------------

test_that("exactly 50 x n entries: no page n+1 is asked (#73)", {
  local_fresh_state()
  srv <- serve(fake_response(200, report_page(1:50, total = 100)),
               fake_response(200, report_page(51:100, total = 100)))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  expect_equal(nrow(suppressMessages(report())), 100)
  expect_equal(srv$calls(), 2)
})

test_that("a deletion during pagination (total_count decreases) triggers a new read (#73)", {
  local_fresh_state()
  srv <- serve(fake_response(200, report_page(1:50, total = 60)),
               fake_response(200, report_page(52:60, total = 59)),
               fake_response(200, report_page(c(1:49, 51), total = 59)),
               fake_response(200, report_page(52:60, total = 59)))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  out <- suppressMessages(report())
  expect_equal(nrow(out), 59)
  expect_false(50 %in% out$id)
})

test_that("duplicates with a stable total (an entry missed) trigger a new read (#73)", {
  local_fresh_state()
  srv <- serve(fake_response(200, report_page(1:50, total = 60)),
               fake_response(200, report_page(50:59, total = 60)), # 50 en double, 60 manque
               fake_response(200, report_page(1:50, total = 60)),
               fake_response(200, report_page(51:60, total = 60)))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  out <- suppressMessages(report())
  expect_equal(sort(out$id), 1:60)
  expect_equal(srv$calls(), 4)
})

test_that("an answer without total_count is an error, not an empty period (#73)", {
  local_fresh_state()
  srv <- serve(fake_response(200, list(data = list())))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  expect_error(suppressMessages(report()), "no total_count")
})

test_that("the url asks the whole period, until included, grouped by user (#73)", {
  local_fresh_state()
  urls <- c()
  local_mocked_bindings(
    toggl_http_get = function(url, api_token) { urls <<- c(urls, url); fake_response(200, report_page(1:2, total = 2)) },
    toggl_sleep = function(s) NULL
  )
  suppressMessages(report())
  expect_match(urls, "/reports/api/v2/details\\?workspace_id=1&")
  expect_match(urls, "since=2026-10-01T00:00:00&until=2026-10-31T00:00:00")
  expect_match(urls, "grouping=users&page=1$")
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
  srv <- serve(fake_response(200, report_page(1:3, total = 3)))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
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
