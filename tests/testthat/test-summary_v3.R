# ---- #76 : resume, hebdomadaire et dashboard via Reports API v3 -------------------------

# Noms des utilisateurs, projets et clients (recherches v9 simulees)
local_names <- function(env = parent.frame()) {
  local_mocked_bindings(
    get_workspace_users = function(...) dplyr::tibble(id = c(10L, 20L), fullname = c("Alice", "Bob")),
    get_all_projects = function(...) dplyr::tibble(id = c(1L, 2L), name = c("presta_a", "presta_b"),
                                                    client_id = c(5L, NA), color = c("#06aaf5", "#d94182")),
    get_all_client_info = function(...) dplyr::tibble(id = 5L, name = "acme"),
    .env = env
  )
}

# POST simule : enregistre (url, corps) et repond selon l'endpoint
local_reports <- function(answers, env = parent.frame()) {
  seen <- list()
  local_mocked_bindings(POST = function(url, ..., body = NULL) {
    seen[[length(seen) + 1]] <<- list(url = url, body = jsonlite::fromJSON(body, simplifyVector = FALSE))
    key <- sub(".*/workspace/[0-9]+", "", url)
    if (is.null(answers[[key]])) stop("unexpected url: ", url)
    fake_response(200, answers[[key]])
  }, toggl_sleep = function(s) NULL, .env = env)
  local_fresh_state(env)
  function() seen
}

entry_group <- function(title, seconds, start = "2026-09-04T13:28:52Z") {
  list(id = NULL, title = title, seconds = seconds, local_start = start)
}

test_that("get_summary_report: one row per user, time and items in ms, from Reports v3 (#76)", {
  local_names()
  seen <- local_reports(list(`/summary/time_entries` = list(groups = list(
    list(id = 10L, sub_groups = list(entry_group("point", 1800), entry_group("dev", 5400))),
    list(id = 20L, sub_groups = list(entry_group(NULL, 600)))))))
  out <- get_summary_report(api_token = "t", workspace_id = 1, since = "2026-09-01", until = "2026-09-30")
  expect_equal(seen()[[1]]$url, "https://api.track.toggl.com/reports/api/v3/workspace/1/summary/time_entries")
  b <- seen()[[1]]$body
  expect_equal(c(b$start_date, b$end_date, b$grouping, b$sub_grouping),
               c("2026-09-01", "2026-09-30", "users", "time_entries"))
  expect_named(out, c("id", "user", "time", "items"))
  expect_equal(out$id, c(10L, 20L))
  expect_equal(out$user, c("Alice", "Bob"))
  expect_equal(out$time, c(7200000, 600000))
  expect_equal(out$items[[1]]$title$time_entry, c("point", "dev"))
  expect_equal(out$items[[1]]$time, c(1800000, 5400000))
  expect_equal(out$items[[1]]$local_start, rep("2026-09-04T13:28:52", 2))
  expect_true(is.na(out$items[[2]]$title$time_entry))
})

test_that("get_dashboard: synthese per project with names and clients, tasks per project (#76)", {
  local_names()
  local_reports(list(`/summary/time_entries` = list(groups = list(
    list(id = 1L, sub_groups = list(entry_group("t1", 3600), entry_group("t2", 1800))),
    list(id = 2L, sub_groups = list(entry_group("t3", 600))),
    list(id = NULL, sub_groups = list(entry_group("libre", 60)))))))
  d <- get_dashboard(api_token = "t", workspace_id = 1, since = "2026-09-01", until = "2026-09-30")
  expect_s3_class(d, "toggl")
  expect_named(d$synthese, c("id", "project", "client", "time"))
  expect_equal(d$synthese$id, c(1L, 2L, NA))
  expect_equal(d$synthese$project, c("presta_a", "presta_b", NA))
  expect_equal(d$synthese$client, c("acme", NA, NA))
  expect_equal(d$synthese$time, c(5400000, 600000, 60000))
  expect_equal(names(d$tache)[1:2], c("presta_a", "presta_b"))
  expect_equal(d$tache$presta_a$title, c("t1", "t2"))
  expect_equal(d$tache$presta_a$time, c(3600000, 1800000))
})

test_that("get_dashboard sends a projects / time_entries summary request (#76)", {
  local_names()
  seen <- local_reports(list(`/summary/time_entries` = list(groups = list())))
  d <- get_dashboard(api_token = "t", workspace_id = 1, since = "2026-09-01", until = "2026-09-30")
  b <- seen()[[1]]$body
  expect_equal(c(b$grouping, b$sub_grouping), c("projects", "time_entries"))
  expect_equal(nrow(d$synthese), 0)
  expect_named(d$synthese, c("id", "project", "client", "time"))
})

test_that("get_weekly_report: one row per project, daily totals and per user details, in ms (#76)", {
  local_names()
  seen <- local_reports(list(`/weekly/time_entries` = list(
    list(user_id = 10L, project_id = 1L, seconds = list(3600, 0, 0, 1800, 0, 0, 0)),
    list(user_id = 20L, project_id = 1L, seconds = list(0, 600, 0, 0, 0, 0, 0)),
    list(user_id = 20L, project_id = NULL, seconds = list(0, 0, 0, 0, 60, 0, 0)))))
  w <- get_weekly_report(api_token = "t", workspace_id = 1, since = "2026-09-07", until = "2026-09-13")
  expect_equal(seen()[[1]]$url, "https://api.track.toggl.com/reports/api/v3/workspace/1/weekly/time_entries")
  expect_named(w, c("pid", "title", "totals", "details"))
  expect_equal(w$pid, c(1L, NA))
  expect_equal(w$title$project, c("presta_a", NA))
  expect_equal(w$title$client, c("acme", NA))
  expect_equal(w$totals[[1]], c(3600000, 600000, NA, 1800000, NA, NA, NA, 6000000))
  expect_equal(w$details[[1]]$uid, c(10L, 20L))
  expect_equal(w$details[[1]]$title$user, c("Alice", "Bob"))
  expect_equal(w$details[[1]]$totals[[2]], c(NA, 600000, NA, NA, NA, NA, NA, 600000))
})

test_that("get_weekly_report: a week without any entry gives an empty tibble (revue #85)", {
  local_names()
  local_reports(list(`/weekly/time_entries` = list()))
  w <- get_weekly_report(api_token = "t", workspace_id = 1, since = "2026-09-07", until = "2026-09-13")
  expect_named(w, c("pid", "title", "totals", "details"))
  expect_equal(nrow(w), 0)
  expect_named(w$title, c("project", "client", "color", "hex_color"))
})

test_that("summary and weekly requests disable the rounding of the account (revue #85)", {
  local_names()
  seen <- local_reports(list(
    `/weekly/time_entries` = list(),
    `/summary/time_entries` = list(groups = list())
  ))
  get_weekly_report(api_token = "t", workspace_id = 1, since = "2026-09-07", until = "2026-09-13")
  get_summary_report(api_token = "t", workspace_id = 1, since = "2026-09-07", until = "2026-09-13")
  get_dashboard(api_token = "t", workspace_id = 1, since = "2026-09-07", until = "2026-09-13")
  bodies <- lapply(seen(), `[[`, "body")
  expect_length(bodies, 3)
  for (b in bodies) {
    expect_equal(b$rounding, 0)
  }
})

test_that("the names of the reports include the archived projects (revue #85)", {
  local_fresh_state()
  urls <- c()
  local_mocked_bindings(toggl_http_get = function(url, api_token) {
    urls <<- c(urls, url)
    fake_response(200, list())
  }, toggl_sleep = function(s) NULL)
  n <- toggl_report_names(api_token = "t", workspace_id = 9, users = FALSE)
  expect_match(urls, "/workspaces/9/projects\\?page=1&per_page=200&active=both", all = FALSE)
  expect_equal(nrow(n$projects), 0)
  urls <- c()
  client_names(api_token = "t", workspace_id = 9)
  expect_match(urls, "/workspaces/9/projects\\?page=1&per_page=200&active=both", all = FALSE)
})

# ---- #12 : filtre par membre de l'equipe ---------------------------------------------------

test_that("get_all_project_names and get_dashboard can be limited to some users (#12)", {
  local_names()
  seen <- local_reports(list(`/summary/time_entries` = list(groups = list(
    list(id = 1L, sub_groups = list(entry_group("t1", 3600)))))))
  p <- get_all_project_names(api_token = "t", workspace_id = 1, since = "2026-09-01",
                             until = "2026-09-30", user_ids = c(10, 20))
  expect_equal(p, "presta_a")
  expect_equal(unlist(seen()[[1]]$body$user_ids), c(10, 20))
  get_dashboard(api_token = "t", workspace_id = 1, since = "2026-09-01", until = "2026-09-30")
  expect_null(seen()[[2]]$body$user_ids)
})
