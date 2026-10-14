# ---- I10 (#54) : les ecritures verifient le statut HTTP ---------------------------

test_that("a write answered by an error raises the API message (#54)", {
  local_fresh_state()
  srv <- serve_write(fake_response(400, "description too long"))
  local_mocked_bindings(POST = srv$send, toggl_sleep = function(s) NULL)
  expect_error(toggl_request("POST", "https://x/time_entries", api_token = "t", body = "{}"),
               "toggl API error 400.*description too long")
})

test_that("a POST is not retried after a 5xx: it may have been processed (#54)", {
  local_fresh_state()
  srv <- serve_write(fake_response(502), fake_response(200, list(id = 1L)))
  local_mocked_bindings(POST = srv$send, toggl_sleep = function(s) NULL)
  expect_error(toggl_request("POST", "u", api_token = "t", body = "{}"), "502")
  expect_equal(srv$calls(), 1)
})

test_that("a POST is not retried after a network error either (#54)", {
  local_fresh_state()
  srv <- serve_write(simpleError("Timeout was reached"), fake_response(200, list(id = 1L)))
  local_mocked_bindings(POST = srv$send, toggl_sleep = function(s) NULL)
  expect_error(toggl_request("POST", "u", api_token = "t", body = "{}"), "may have been processed")
  expect_equal(srv$calls(), 1)
})

test_that("a POST is retried after a 429: it was not processed (#54)", {
  local_fresh_state()
  srv <- serve_write(fake_response(429, headers = list(`retry-after` = "3")),
                     fake_response(200, list(id = 7L)))
  local_mocked_bindings(POST = srv$send, toggl_sleep = function(s) NULL)
  expect_equal(toggl_request("POST", "u", api_token = "t", body = "{}")$content$id, 7L)
})

test_that("an idempotent write (PUT / PATCH) is retried after a 5xx (#54)", {
  local_fresh_state()
  srv <- serve_write(fake_response(503), fake_response(200, list(id = 7L)))
  local_mocked_bindings(PUT = srv$send, toggl_sleep = function(s) NULL)
  expect_equal(toggl_request("PUT", "u", api_token = "t", body = "{}")$content$id, 7L)
})

test_that("a write respects a known exhausted quota: nothing is sent (#54)", {
  local_fresh_state()
  toggl_state_set(toggl_quota_key("t"), Sys.time() + 1800)
  srv <- serve_write(fake_response(200, list(id = 1L)))
  local_mocked_bindings(POST = srv$send, toggl_sleep = function(s) NULL)
  expect_error(toggl_request("POST", "u", api_token = "t", body = "{}"), "no request sent")
  expect_equal(srv$calls(), 0)
})

test_that("toggl_start fails with the API error instead of `$ operator is invalid` (#54)", {
  local_fresh_state()
  notes <- c()
  srv <- serve_write(fake_response(403, "Incorrect username and/or password"))
  local_mocked_bindings(POST = srv$send, toggl_sleep = function(s) NULL, get_project_id = function(...) 1L,
                        notification = function(title, msg) notes <<- c(notes, title))
  expect_error(toggl_start("task", api_token = "t", workspace_id = 1), "403")
  expect_match(notes, "ERROR")
})

test_that("toggl_stop only says STOP when the API accepted it (#54)", {
  local_fresh_state()
  notes <- c()
  srv <- serve_write(fake_response(404, "time entry not found"))
  local_mocked_bindings(PATCH = srv$send, toggl_sleep = function(s) NULL,
                        notification = function(title, msg) notes <<- c(notes, title))
  expect_error(toggl_stop(current = list(id = 9L, description = "task", start = "2026-10-14T08:00:00Z"),
                          api_token = "t", workspace_id = 1), "404")
  expect_false(any(grepl("STOP", notes)))
})

test_that("create_client returns the created client (#54)", {
  local_fresh_state()
  srv <- serve_write(fake_response(200, list(id = 5L, name = "acme")))
  local_mocked_bindings(POST = srv$send, toggl_sleep = function(s) NULL)
  expect_equal(suppressMessages(create_client("acme", api_token = "t", workspace_id = 1))$id, 5L)
})
