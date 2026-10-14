# ---- M1 (#59) : get_all_projects ne boucle pas sans fin -----------------------------

projects <- function(ids) lapply(ids, function(i) list(id = i, name = paste0("p", i), active = TRUE))

test_that("pages are read until a short one (#59)", {
  local_fresh_state()
  srv <- serve(fake_response(200, projects(1:200)), fake_response(200, projects(201:250)))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  expect_equal(nrow(get_all_projects(api_token = "t", workspace_id = 1)), 250)
  expect_equal(srv$calls(), 2)
})

test_that("exactly 200 projects: the empty next page ends the loop (#59)", {
  local_fresh_state()
  srv <- serve(fake_response(200, projects(1:200)), fake_response(200, list()))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  expect_equal(nrow(get_all_projects(api_token = "t", workspace_id = 1)), 200)
})

test_that("an API ignoring `page` (same page again) stops the loop with an error (#59)", {
  local_fresh_state()
  srv <- serve(fake_response(200, projects(1:200)), fake_response(200, projects(1:200)),
               fake_response(200, projects(1:200)))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  expect_error(get_all_projects(api_token = "t", workspace_id = 1), "same page")
  expect_equal(srv$calls(), 2)
})

test_that("the number of pages is bounded (#59)", {
  local_fresh_state()
  n <- 0
  local_mocked_bindings(
    toggl_http_get = function(url, api_token) { n <<- n + 1; fake_response(200, projects((n - 1) * 200 + 1:200)) },
    toggl_sleep = function(s) NULL
  )
  withr::local_options(togglr.max_pages = 5)
  expect_error(get_all_projects(api_token = "t", workspace_id = 1), "more than 5 pages")
  expect_equal(n, 5)
})
