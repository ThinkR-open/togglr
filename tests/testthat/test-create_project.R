# ---- I11 (#55) : toggl_create_project -------------------------------------------

local_project_api <- function(env = parent.frame()) {
  bodies <- list()
  created <- character(0)
  local_mocked_bindings(
    POST = function(url, ..., body) {
      bodies[[length(bodies) + 1]] <<- jsonlite::fromJSON(body)
      fake_response(200, list(id = 77L))
    },
    get_project_id = function(...) NULL,
    client_name_to_id = function(name, ...) if (name == "acme") 5L else integer(0),
    create_client = function(name, ...) { created <<- c(created, name); list(id = 8L, name = name) },
    toggl_sleep = function(s) NULL,
    .env = env
  )
  local_fresh_state(env)
  list(body = function() bodies[[length(bodies)]], created = function() created, n = function() length(bodies))
}

test_that("without client (NULL or empty), the project is created without client (#55)", {
  api <- local_project_api()
  expect_equal(toggl_create_project("p", api_token = "t", workspace_id = 1, client = NULL), 77L)
  expect_false("client_id" %in% names(api$body()))
  expect_equal(toggl_create_project("p", api_token = "t", workspace_id = 1, client = ""), 77L)
  expect_false("client_id" %in% names(api$body()))
  expect_length(api$created(), 0)
})

test_that("an existing client is not created again, and its id is sent (#55)", {
  api <- local_project_api()
  toggl_create_project("p", api_token = "t", workspace_id = 1, client = "acme")
  expect_length(api$created(), 0)
  expect_equal(api$body()$client_id, 5L)
  expect_false("client" %in% names(api$body()))
})

test_that("a new client is created once, and the new id is sent (#55)", {
  api <- local_project_api()
  suppressMessages(toggl_create_project("p", api_token = "t", workspace_id = 1, client = "newco"))
  expect_equal(api$created(), "newco")
  expect_equal(api$body()$client_id, 8L)
})

test_that("private, color and active are sent in the v9 format (#55)", {
  api <- local_project_api()
  toggl_create_project("p", api_token = "t", workspace_id = 1, private = TRUE,
                       color = "#06aaf5", active = FALSE)
  b <- api$body()
  expect_true(b$is_private)
  expect_equal(b$color, "#06aaf5")
  expect_false(b$active)
  expect_equal(b$name, "p")
  expect_null(b$project) # ancien corps imbrique v8
})

test_that("an existing project is not created again (#55)", {
  api <- local_project_api()
  local_mocked_bindings(get_project_id = function(...) 12L)
  expect_warning(id <- toggl_create_project("p", api_token = "t", workspace_id = 1), "already exist")
  expect_equal(id, 12L)
  expect_equal(api$n(), 0)
})

test_that("a project with a client is created in a workspace without any client (revue #75)", {
  local_fresh_state()
  created <- character(0)
  srv <- serve(fake_response(200, NULL))
  local_mocked_bindings(
    toggl_http_get = srv$get,
    POST = function(url, ...) fake_response(200, list(id = 77L)),
    get_project_id = function(...) NULL,
    create_client = function(name, ...) { created <<- c(created, name); list(id = 8L) },
    toggl_sleep = function(s) NULL
  )
  expect_equal(toggl_create_project("p", api_token = "t", workspace_id = 1,
                                    client = "without client"), 77L)
  expect_equal(created, "without client")
})

test_that("get_all_client_info keeps id and name for a workspace without client (revue #75)", {
  local_fresh_state()
  srv <- serve(fake_response(200, list()))
  local_mocked_bindings(toggl_http_get = srv$get, toggl_sleep = function(s) NULL)
  out <- get_all_client_info(api_token = "t", workspace_id = 1)
  expect_equal(nrow(out), 0)
  expect_true(all(c("id", "name") %in% names(out)))
})
