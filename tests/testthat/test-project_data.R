# ---- #28 : get_project_data() --------------------------------------------------------------

test_that("get_project_data returns the data of one project, from its id (#28)", {
  local_fresh_state()
  urls <- c()
  local_mocked_bindings(toggl_http_get = function(url, api_token) {
    urls <<- c(urls, url)
    fake_response(200, list(id = 42L, name = "presta_a", client_id = 5L, active = TRUE, billable = FALSE))
  }, toggl_sleep = function(s) NULL)
  p <- get_project_data(42, api_token = "t", workspace_id = 1)
  expect_equal(urls, "https://api.track.toggl.com/api/v9/workspaces/1/projects/42")
  expect_equal(p$name, "presta_a")
  expect_equal(p$client_id, 5L)
})

test_that("get_project_data fails clearly on an unknown project (#28)", {
  local_fresh_state()
  local_mocked_bindings(toggl_http_get = function(url, api_token) fake_response(404, "Project not found"),
                        toggl_sleep = function(s) NULL)
  expect_error(get_project_data(42, api_token = "t", workspace_id = 1), "404.*Project not found")
})
