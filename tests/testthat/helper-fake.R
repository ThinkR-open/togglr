# Build an httr response without network
fake_response <- function(status = 200, body = list(), headers = list()) {
  headers <- c(list(`content-type` = "application/json; charset=utf-8"), headers)
  structure(
    list(
      url = "https://api.track.toggl.com/fake",
      status_code = status,
      headers = structure(headers, class = c("insensitive", "list")),
      content = charToRaw(as.character(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")))
    ),
    class = "response"
  )
}

# A queue of responses served one by one by the mocked toggl_http_get()
serve <- function(...) {
  queue <- list(...)
  calls <- 0
  list(
    get = function(url, api_token) {
      calls <<- calls + 1
      if (calls > length(queue)) stop("unexpected extra call: ", url)
      queue[[calls]]
    },
    calls = function() calls
  )
}

# One page of the reports API v2 /details
report_page <- function(ids, total, per_page = 50, total_grand = NULL) {
  list(
    total_count = total,
    total_grand = total_grand,
    per_page = per_page,
    data = lapply(ids, function(i) list(
      id = i, pid = 1L, uid = 2L, description = paste("entry", i),
      start = "2026-10-12T09:00:00+02:00", end = "2026-10-12T10:00:00+02:00",
      updated = "2026-10-12T10:00:00+02:00", dur = 3600000L, user = "Someone",
      project = "presta_x_"
    ))
  )
}

# Etat de session vierge pour un test (espacement, quota), et sans fichier d'etat
local_fresh_state <- function(env = parent.frame()) {
  withr::local_options(togglr.state_file = NULL, .local_envir = env)
  rm(list = ls(.togglr_env, all.names = TRUE), envir = .togglr_env)
  withr::defer(rm(list = ls(.togglr_env, all.names = TRUE), envir = .togglr_env), envir = env)
}

# Simule un autre processus R : la memoire de session est perdue
forget_session <- function() rm(list = ls(.togglr_env, all.names = TRUE), envir = .togglr_env)

# File de reponses pour les ecritures (POST / PUT / PATCH / DELETE mockes)
serve_write <- function(...) {
  queue <- list(...)
  calls <- 0
  list(
    send = function(url, ...) {
      calls <<- calls + 1
      if (calls > length(queue)) stop("unexpected extra call: ", url)
      r <- queue[[calls]]
      if (inherits(r, "error")) stop(r)
      r
    },
    calls = function() calls
  )
}

# Enregistre les periodes demandees a fetch_detailled_period (sans reseau)
local_periods <- function(env = parent.frame()) {
  asked <- character(0)
  local_mocked_bindings(
    fetch_detailled_period = function(api_token, workspace_id, since, until, user_agent) {
      asked <<- c(asked, paste(format(since), format(until)))
      normalize_detailled_report(tibble::tibble(id = length(asked)))
    },
    .env = env
  )
  function() sort(asked)
}

# Rapport detaille d'octobre 2026 (workspace 1)
report <- function(...) {
  get_detailled_report(api_token = "t", workspace_id = 1,
                       since = as.Date("2026-10-01"), until = as.Date("2026-10-31"), ...)
}
