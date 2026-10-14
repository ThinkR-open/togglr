# httr::verbose() prints the request headers, including
# "Authorization: Basic <base64(token:api_token)>": the token ends up in logs.
is_verbose <- function(a) inherits(a, "request") && isTRUE(a$options$verbose)

capture_post <- function() {
  seen <- list()
  list(
    post = function(...) {
      seen <<- c(seen, list(list(...)))
      fake_response(200, list(id = 1L))
    },
    called = function() length(seen) > 0,
    verbose_used = function() any(vapply(unlist(seen, recursive = FALSE), is_verbose, logical(1)))
  )
}

test_that("create_client never turns verbose on (the token would be printed)", {
  cap <- capture_post()
  local_mocked_bindings(POST = cap$post)
  suppressMessages(create_client("acme", api_token = "secret", workspace_id = 1))
  expect_true(cap$called())
  expect_false(cap$verbose_used())
})

test_that("toggl_create_project never turns verbose on (the token would be printed)", {
  cap <- capture_post()
  local_mocked_bindings(POST = cap$post, get_project_id = function(...) NULL,
                        create_client = function(...) NULL, client_name_to_id = function(...) 7L)
  suppressMessages(toggl_create_project("presta_x", client = "acme", api_token = "secret",
                                        workspace_id = 1))
  expect_true(cap$called())
  expect_false(cap$verbose_used())
})
