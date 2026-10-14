# ---- I12 (#56) : toggl_update_entries ----------------------------------------------

local_put_api <- function(env = parent.frame()) {
  calls <- list()
  local_mocked_bindings(
    PUT = function(url, ..., body) {
      calls[[length(calls) + 1]] <<- list(url = url, body = jsonlite::fromJSON(body))
      fake_response(200, list(id = as.integer(sub(".*/", "", url))))
    },
    toggl_sleep = function(s) NULL,
    .env = env
  )
  local_fresh_state(env)
  function() calls
}

test_that("one entry is updated with one PUT on its own url (#56)", {
  calls <- local_put_api()
  res <- toggl_update_entries(11, description = "new", api_token = "t", workspace_id = 1)
  expect_length(calls(), 1)
  expect_match(calls()[[1]]$url, "/workspaces/1/time_entries/11$")
  expect_equal(calls()[[1]]$body$description, "new")
  expect_equal(res$id, 11L)
})

test_that("several entries get one PUT each, never a comma separated id list (#56)", {
  calls <- local_put_api()
  res <- toggl_update_entries(c(11, 12), description = "new", api_token = "t", workspace_id = 1)
  urls <- vapply(calls(), `[[`, "", "url")
  expect_match(urls, "/time_entries/1[12]$")
  expect_false(any(grepl(",", urls)))
  expect_equal(vapply(res, `[[`, 0L, "id"), c(11L, 12L))
})

test_that("stop is sent, with its time zone offset (#56)", {
  calls <- local_put_api()
  toggl_update_entries(11, stop = as.POSIXct("2026-07-01 10:00:00", tz = "UTC"),
                       api_token = "t", workspace_id = 1)
  expect_equal(lubridate::ymd_hms(calls()[[1]]$body$stop),
               as.POSIXct("2026-07-01 10:00:00", tz = "UTC"))
})

test_that("the project is sent as project_id, and read-only fields are not sent (#56)", {
  calls <- local_put_api()
  toggl_update_entries(11, pid = 3L, tags = c("a", "b"), api_token = "t", workspace_id = 1)
  b <- calls()[[1]]$body
  expect_equal(b$project_id, 3L)
  expect_equal(b$tags, c("a", "b"))
  expect_false("at" %in% names(b))
})
