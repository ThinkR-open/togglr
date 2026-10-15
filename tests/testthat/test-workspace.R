# ---- #23 : workspace par defaut, liste des workspaces ------------------------------------

local_accounts <- function(me, workspaces, env = parent.frame()) {
  urls <- c()
  local_mocked_bindings(toggl_http_get = function(url, api_token) {
    urls <<- c(urls, url)
    if (grepl("/me$", url)) return(fake_response(200, me))
    if (grepl("/workspaces$", url)) return(fake_response(200, workspaces))
    stop("unexpected url: ", url)
  }, toggl_sleep = function(s) NULL, .env = env)
  local_fresh_state(env)
  function() urls
}

ws <- list(list(id = 11L, name = "Perso"), list(id = 22L, name = "ThinkR"))

test_that("get_workspace_id returns the default workspace of the account, not the first one (#23)", {
  local_accounts(me = list(id = 1L, default_workspace_id = 22L), workspaces = ws)
  expect_equal(get_workspace_id(api_token = "t"), 22L)
})

test_that("without a default workspace, the first one is used (#23)", {
  local_accounts(me = list(id = 1L, default_workspace_id = NULL), workspaces = ws)
  expect_equal(get_workspace_id(api_token = "t"), 11L)
})

test_that("get_workspaces lists every workspace, the default one flagged (#23)", {
  local_accounts(me = list(id = 1L, default_workspace_id = 22L), workspaces = ws)
  w <- get_workspaces(api_token = "t")
  expect_equal(w$id, c(11L, 22L))
  expect_equal(w$name, c("Perso", "ThinkR"))
  expect_equal(w$default, c(FALSE, TRUE))
})
