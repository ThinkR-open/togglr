# ---- M3 (#61) : dates acceptees et controlees --------------------------------------

# Capture l'url demandee puis interrompt l'appel
capture_url <- function(env = parent.frame()) {
  urls <- c()
  local_mocked_bindings(toggl_GET = function(url, ...) { urls <<- c(urls, url); stop("captured") },
                        toggl_sleep = function(s) NULL, .env = env)
  local_fresh_state(env)
  function() urls
}

report_functions <- list(
  detailled = function(...) get_detailled_report_paged(api_token = "t", workspace_id = 1, ...),
  summary   = function(...) get_summary_report(api_token = "t", workspace_id = 1, ...),
  weekly    = function(...) get_weekly_report(api_token = "t", workspace_id = 1, ...),
  dashboard = function(...) get_dashboard(api_token = "t", workspace_id = 1, ...)
)

test_that("dates given as text AAAA-MM-JJ are accepted by every report (#61)", {
  for (f in report_functions) {
    urls <- capture_url()
    expect_error(f(since = "2026-10-01", until = "2026-10-31"), "captured")
    expect_match(urls(), "since=2026-10-01")
    expect_match(urls(), "until=2026-10-31")
  }
})

test_that("a date-time is taken as its day in its own time zone (#61)", {
  urls <- capture_url()
  expect_error(get_summary_report(api_token = "t", workspace_id = 1,
                                  since = as.POSIXct("2026-10-01 00:30:00", tz = "Europe/Paris"),
                                  until = as.POSIXct("2026-10-31 23:30:00", tz = "Europe/Paris")),
               "captured")
  expect_match(urls(), "since=2026-10-01&until=2026-10-31")
})

test_that("an unreadable date or an inverted period fails clearly, before any request (#61)", {
  for (f in report_functions) {
    urls <- capture_url()
    expect_error(f(since = "01/10/2026", until = "2026-10-31"), "`since`.*Date")
    expect_error(f(since = 20261001, until = "2026-10-31"), "`since`.*Date")
    expect_error(f(since = "2026-10-31", until = "2026-10-01"), "`since`.*after.*`until`")
    expect_length(urls(), 0)
  }
})

test_that("get_detailled_report checks its dates too (#61)", {
  urls <- capture_url()
  expect_error(get_detailled_report(api_token = "t", workspace_id = 1, since = "2026-13-01"), "`since`.*Date")
  expect_length(urls(), 0)
})
