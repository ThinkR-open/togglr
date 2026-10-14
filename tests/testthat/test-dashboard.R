# ---- M13 (#71) : get_dashboard (any_of, user agent) -------------------------------------

summary_answer <- function(with_colors = TRUE) {
  title <- list(project = "p1", client = "acme")
  if (with_colors) title <- c(title, list(color = "0", hex_color = "#06aaf5"))
  list(data = list(list(id = 1L, title = title, time = 3600000L,
                        items = list(list(title = list(time_entry = "task"), time = 3600000L)))))
}

local_dashboard_api <- function(answer, env = parent.frame()) {
  urls <- c()
  local_mocked_bindings(toggl_http_get = function(url, api_token) { urls <<- c(urls, url); fake_response(200, answer) },
                        toggl_sleep = function(s) NULL, .env = env)
  local_fresh_state(env)
  function() urls
}

test_that("get_dashboard drops the color columns, and works without them (#71)", {
  local_dashboard_api(summary_answer(TRUE))
  d <- get_dashboard(api_token = "t", workspace_id = 1, since = "2026-10-01", until = "2026-10-31")
  expect_false(any(c("color", "hex_color") %in% names(d$synthese)))
  local_dashboard_api(summary_answer(FALSE))
  expect_no_warning(d <- get_dashboard(api_token = "t", workspace_id = 1, since = "2026-10-01", until = "2026-10-31"))
  expect_equal(d$synthese$project, "p1")
})

test_that("every report sends the togglr user agent, not a hardcoded api_test (#71)", {
  urls <- local_dashboard_api(summary_answer(TRUE))
  get_dashboard(api_token = "t", workspace_id = 1, since = "2026-10-01", until = "2026-10-31")
  expect_false(grepl("api_test", urls()))
  expect_match(urls(), paste0("user_agent=togglr%2F", utils::packageVersion("togglr")), fixed = TRUE)
})

test_that("requests carry a User-Agent header (#71)", {
  seen <- NULL
  local_mocked_bindings(GET = function(...) { seen <<- list(...); fake_response(200) })
  toggl_http_get("u", "t")
  ua <- unlist(lapply(Filter(function(a) inherits(a, "request"), seen), function(r) r$options$useragent))
  expect_match(ua, "^togglr/")
})
