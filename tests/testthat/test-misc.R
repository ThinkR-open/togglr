# ---- A8 (#84), #33 : fonctions jusque-la sans test ------------------------------------------

test_that("notification shows title and message", {
  expect_message(notification("task START", c("at :", "10:00")), "task START - at :10:00")
})

test_that("open_toggl_* open the Toggl pages in the browser", {
  opened <- c()
  local_mocked_bindings(browseURL = function(url, ...) opened <<- c(opened, url))
  suppressMessages({ open_toggl_website_profile(); open_toggl_website_app() })
  expect_equal(opened, c("https://track.toggl.com/profile", "https://track.toggl.com/timer"))
})

test_that("get_context / get_context_project name the RStudio project, or say there is none", {
  local_mocked_bindings(getActiveProject = function() "/home/me/myproj")
  expect_equal(get_context(), "projet R myproj")
  expect_equal(get_context_project(), "myproj")
  local_mocked_bindings(getActiveProject = function() stop("RStudio not running"))
  expect_equal(get_context(), "I'm using R")
  expect_equal(get_context_project(), "sans projet")
})

test_that("get_workspace_users returns one row per user of the workspace", {
  local_fresh_state()
  urls <- c()
  local_mocked_bindings(toggl_http_get = function(url, api_token) {
    urls <<- c(urls, url)
    fake_response(200, list(list(id = 1L, fullname = "Alice", email = "a@x"),
                            list(id = 2L, fullname = "Bob", email = "b@x")))
  }, toggl_sleep = function(s) NULL)
  u <- get_workspace_users(api_token = "t", workspace_id = 9)
  expect_equal(urls, "https://api.track.toggl.com/api/v9/workspaces/9/users")
  expect_equal(u$fullname, c("Alice", "Bob"))
})

test_that("with togglr.state_file, real concurrent processes are spaced (#84)", {
  skip_on_cran()
  skip_if_not_installed("callr")
  skip_if_not_installed("pkgload")
  state <- withr::local_tempfile()
  lib <- .libPaths()
  src <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  from_sources <- length(list.files(file.path(src, "R"), pattern = "\\.R$")) > 0
  worker <- function(state, lib, src, from_sources) {
    .libPaths(lib)
    if (from_sources) {
      pkgload::load_all(src, quiet = TRUE)
    } else {
      loadNamespace("togglr", lib.loc = lib)
    }
    pace <- get("toggl_pace", envir = asNamespace("togglr"))
    options(togglr.state_file = state)
    t <- c()
    for (i in 1:3) {
      pace(min_interval = 1)
      t <- c(t, as.numeric(Sys.time()))
    }
    t
  }
  args <- list(state = state, lib = lib, src = src, from_sources = from_sources)
  p <- lapply(1:2, function(i) {
    callr::r_bg(worker, args = args)
  })
  for (x in p) x$wait(60000)
  times <- sort(unlist(lapply(p, function(x) x$get_result())))
  expect_length(times, 6)
  expect_true(all(diff(times) > 0.9)) # 6 creneaux d'1 s, jamais deux envois ensemble
})
