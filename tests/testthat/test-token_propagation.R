# ---- I7 (#51) : le token et le workspace fournis sont utilises partout ----------

# Faux serveur : enregistre (url, token) de chaque requete et repond selon l'url
fake_api <- function() {
  log <- data.frame(url = character(0), token = character(0))
  answer <- function(url) {
    if (grepl("/me$", url)) return(list(id = 1L, default_workspace_id = 111L))
    if (grepl("/workspaces$", url)) return(list(list(id = 111L)))
    if (grepl("/projects", url)) return(list(list(id = 1L, name = "p1", active = TRUE, client_id = 5L)))
    if (grepl("/clients", url)) return(list(list(id = 5L, name = "acme")))
    if (grepl("/me/time_entries/current", url)) {
      return(list(id = 9L, description = "d", start = "2026-10-14T08:00:00+00:00",
                  at = "2026-10-14T08:00:00+00:00"))
    }
    if (grepl("/me/time_entries", url)) {
      return(list(list(id = 3L, project_id = 1L, pid = 1L, wid = 999L, description = "x",
                       start = "2026-10-14T08:00:00+00:00", stop = "2026-10-14T09:00:00+00:00",
                       duration = 3600L, tags = list())))
    }
    stop("unexpected url in fake_api: ", url)
  }
  record <- function(url, token) log[nrow(log) + 1, ] <<- c(url, token)
  token_of <- function(args) {
    a <- Filter(function(x) inherits(x, "request") && !is.null(x$options$userpwd), args)
    sub(":api_token$", "", a[[1]]$options$userpwd)
  }
  list(
    get = function(url, api_token) { record(url, api_token); fake_response(200, answer(url)) },
    write = function(url, ...) { record(url, token_of(list(...))); fake_response(200, list(id = 42L)) },
    log = function() log
  )
}

local_fake_api <- function(env = parent.frame()) {
  api <- fake_api()
  local_mocked_bindings(
    toggl_http_get = api$get, POST = api$write, PUT = api$write, PATCH = api$write,
    toggl_sleep = function(s) NULL, notification = function(...) NULL,
    get_toggl_api_token = function(...) stop("the keyring was used although api_token was given"),
    .env = env
  )
  local_fresh_state(env)
  api
}

expect_only_token <- function(api, token = "tok", workspace_lookup = FALSE) {
  log <- api$log()
  expect_gt(nrow(log), 0)
  expect_true(all(log$token == token), label = paste(unique(log$token), collapse = ","))
  if (!workspace_lookup) expect_false(any(grepl("/workspaces$|/me$", log$url)), label = "useless workspace lookup")
  expect_false(any(grepl("/workspaces/111/", log$url)), label = "default workspace used instead of the given one")
}

test_that("project lookups use the given token and workspace (#51)", {
  api <- local_fake_api()
  expect_equal(get_project_id("p1", api_token = "tok", workspace_id = 999), 1L)
  get_project_id_and_name(api_token = "tok", workspace_id = 999)
  get_client_project(5, api_token = "tok", workspace_id = 999)
  expect_only_token(api)
})

test_that("client lookups use the given token and workspace (#51)", {
  api <- local_fake_api()
  expect_equal(client_name_to_id("acme", api_token = "tok", workspace_id = 999), 5L)
  expect_equal(client_id_to_name(5, api_token = "tok", workspace_id = 999), "acme")
  expect_only_token(api)
})

test_that("get_time_entries uses the given token and workspace (#51)", {
  api <- local_fake_api()
  out <- get_time_entries(api_token = "tok", workspace_id = 999,
                          since = as.POSIXct("2026-10-13", tz = "UTC"),
                          until = as.POSIXct("2026-10-14", tz = "UTC"))
  expect_equal(out$project_name, "p1")
  expect_only_token(api)
})

test_that("dashboard helpers use the given token, also for the default workspace (#51)", {
  api <- local_fake_api()
  local_mocked_bindings(get_dashboard = function(api_token, workspace_id, ...) {
    expect_equal(api_token, "tok")
    list(synthese = data.frame(project = "p1", client = "acme", time = 1000), tache = list())
  })
  get_project_total("p1", api_token = "tok", workspace_id = 999)
  get_all_client_names(api_token = "tok")
  get_current_project_task_detail("p1", api_token = "tok", humain = FALSE)
  expect_only_token(api, workspace_lookup = TRUE)
})

test_that("toggl_start, toggl_stop and toggl_create_project use the given token and workspace (#51)", {
  api <- local_fake_api()
  suppressMessages(suppressWarnings({
    toggl_start("x", client = "acme", project_name = "p1", api_token = "tok", workspace_id = 999)
    toggl_stop(api_token = "tok", workspace_id = 999)
    toggl_create_project("p2", api_token = "tok", workspace_id = 999, client = "acme")
  }))
  expect_only_token(api)
})

test_that("get_toggl_api_token never deletes the stored key, and fails clearly when not interactive (#51)", {
  deleted <- FALSE
  local_mocked_bindings(
    key_get = function(...) stop("keyring locked"),
    key_delete = function(...) deleted <<- TRUE,
    is_interactive = function() FALSE
  )
  expect_error(get_toggl_api_token(), "no toggl API token")
  expect_false(deleted)
})
