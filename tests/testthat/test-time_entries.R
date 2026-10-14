# ---- I13 (#57) : get_time_entries, dates en UTC sans `+` ; tags multiples (#30) ----

local_entries_api <- function(entries, env = parent.frame()) {
  urls <- c()
  local_mocked_bindings(
    toggl_http_get = function(url, api_token) { urls <<- c(urls, url); fake_response(200, entries) },
    get_project_id_and_name = function(...) data.frame(id = 1L, project_name = "p1", active = TRUE),
    toggl_sleep = function(s) NULL,
    .env = env
  )
  local_fresh_state(env)
  function() urls
}

entry <- function(id, tags = list(), tag_ids = list()) {
  list(id = id, project_id = 1L, pid = 1L, wid = 9L, description = "x",
       start = "2026-10-14T08:00:00+00:00", stop = "2026-10-14T09:00:00+00:00",
       duration = 3600L, tags = tags, tag_ids = tag_ids)
}

test_that("the dates are sent in UTC, with a Z and no `+` in the url (#57)", {
  urls <- local_entries_api(list(entry(1L)))
  get_time_entries(api_token = "t", workspace_id = 9,
                   since = as.POSIXct("2026-10-13 10:00:00", tz = "Europe/Paris"),
                   until = as.POSIXct("2026-10-14 10:00:00", tz = "Europe/Paris"))
  expect_false(grepl("+", urls(), fixed = TRUE))
  expect_match(urls(), "start_date=2026-10-13T08:00:00Z")
  expect_match(urls(), "end_date=2026-10-14T08:00:00Z")
})

test_that("entries with several tags give a list column, one element per entry (#30)", {
  local_entries_api(list(entry(1L, tags = list("a", "b"), tag_ids = list(10L, 11L)),
                         entry(2L),
                         entry(3L, tags = list("c"), tag_ids = list(12L))))
  out <- get_time_entries(api_token = "t", workspace_id = 9,
                          since = as.POSIXct("2026-10-13", tz = "UTC"),
                          until = as.POSIXct("2026-10-15", tz = "UTC"))
  expect_equal(nrow(out), 3)
  expect_type(out$tags, "list")
  tags_by_id <- setNames(out$tags, out$id)
  expect_equal(unlist(tags_by_id[["1"]]), c("a", "b"))
  expect_length(unlist(tags_by_id[["2"]]), 0)
  expect_equal(unlist(tags_by_id[["3"]]), "c")
})
