# ---- I13 (#57) : get_time_entries, dates en UTC sans `+` ; tags multiples (#30) ----

local_entries_api <- function(entries, env = parent.frame()) {
  urls <- c()
  local_mocked_bindings(
    toggl_http_get = function(url, api_token) { urls <<- c(urls, url); fake_response(200, entries) },
    get_project_id_and_name = function(...) data.frame(id = 1L, project_name = "p1", active = TRUE),
    get_all_projects = function(...) data.frame(id = 1L, client_id = NA_integer_),
    get_all_client_info = function(...) data.frame(id = integer(0), name = character(0)),
    toggl_sleep = function(s) NULL,
    toggl_today = function() as.Date("2026-10-15"),
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

test_that("since and until may be text dates, as before the 3 months guard (revue Copilot #85)", {
  urls <- local_entries_api(list(entry(1L)))
  out <- get_time_entries(api_token = "t", workspace_id = 9, since = "2026-10-13", until = "2026-10-14")
  expect_equal(nrow(out), 1)
  expect_match(urls(), "start_date=2026-10-1[23]T[0-9:]+Z")
})

test_that("get_time_entries names the archived projects too, and survives a workspace without active project (revue Copilot #85)", {
  urls <- c()
  local_fresh_state()
  local_mocked_bindings(
    toggl_http_get = function(url, api_token) {
      urls <<- c(urls, url)
      fake_response(200, list(entry(1L)))
    },
    get_all_projects = function(api_token, workspace_id, max_pages = 100, active = NULL) {
      if (identical(active, "both")) {
        return(data.frame(id = c(1L, 2L), name = c("old", "p1"), client_id = c(5L, NA),
                          active = c(FALSE, TRUE)))
      }
      data.frame()
    },
    get_all_client_info = function(...) data.frame(id = 5L, name = "acme"),
    toggl_sleep = function(s) NULL,
    toggl_today = function() as.Date("2026-10-15")
  )
  out <- get_time_entries(api_token = "t", workspace_id = 9,
                          since = as.POSIXct("2026-10-13", tz = "UTC"),
                          until = as.POSIXct("2026-10-15", tz = "UTC"))
  expect_equal(out$project_name, "old")
  expect_equal(out$client_name, "acme")
})

# ---- #13 : jamais de troncature a 1000 entrees ; limite de 3 mois de l'API v9 ----------

me_entries <- function(ids, start = "2026-10-14T08:00:00+00:00") lapply(ids, function(i)
  list(id = i, project_id = 1L, pid = 1L, wid = 9L, description = "x", start = start,
       stop = "2026-10-14T09:00:00+00:00", duration = 3600L, tags = list(), tag_ids = list()))

test_that("1000 entries (the cap of /me/time_entries) split the period, nothing is lost (#13)", {
  local_fresh_state()
  local_mocked_bindings(toggl_today = function() as.Date("2026-10-14"))
  urls <- c()
  local_mocked_bindings(
    toggl_http_get = function(url, api_token) {
      urls <<- c(urls, url)
      n <- length(urls)
      fake_response(200, if (n == 1) me_entries(1:1000) else if (n == 2) me_entries(1:700) else me_entries(701:1100))
    },
    get_project_id_and_name = function(...) data.frame(id = 1L, project_name = "p1", active = TRUE),
    get_all_projects = function(...) data.frame(id = 1L, client_id = NA_integer_),
    get_all_client_info = function(...) data.frame(id = integer(0), name = character(0)),
    toggl_sleep = function(s) NULL
  )
  out <- suppressMessages(get_time_entries(api_token = "t", workspace_id = 9,
                                           since = as.POSIXct("2026-10-01", tz = "UTC"),
                                           until = as.POSIXct("2026-10-11", tz = "UTC")))
  expect_equal(nrow(out), 1100)
  expect_length(urls, 3)
  expect_match(urls[2], "start_date=2026-10-01T00:00:00Z&end_date=2026-10-06T00:00:00Z")
  expect_match(urls[3], "start_date=2026-10-06T00:00:00Z&end_date=2026-10-11T00:00:00Z")
})

test_that("more than 3 months back fails before any request, pointing to get_detailled_report (#13)", {
  local_mocked_bindings(toggl_today = function() as.Date("2026-10-14"),
                        toggl_http_get = function(...) stop("no request expected"))
  expect_error(get_time_entries(api_token = "t", workspace_id = 9,
                                since = as.POSIXct("2026-01-01", tz = "UTC"),
                                until = as.POSIXct("2026-02-01", tz = "UTC")),
               "3 months.*get_detailled_report\\(user_ids")
})

# ---- #14 : nom du client ------------------------------------------------------------------

test_that("get_time_entries gives the client name of each entry (#14)", {
  local_fresh_state()
  local_mocked_bindings(
    toggl_http_get = function(url, api_token) fake_response(200, me_entries(1:2)),
    get_project_id_and_name = function(...) data.frame(id = 1L, project_name = "p1", active = TRUE),
    get_all_projects = function(...) data.frame(id = 1L, client_id = 5L),
    get_all_client_info = function(...) data.frame(id = 5L, name = "acme"),
    toggl_sleep = function(s) NULL
  )
  out <- get_time_entries(api_token = "t", workspace_id = 9, since = Sys.time() - 3600, until = Sys.time())
  expect_equal(out$client_name, c("acme", "acme"))
})
