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
    fetch_detailled_period = function(api_token, workspace_id, since, until, ...) {
      asked <<- c(asked, paste(format(since), format(until)))
      normalize_detailled_report(dplyr::tibble(id = length(asked)))
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

# ---- Reports v3 ------------------------------------------------------------------

# Une ligne de /search/time_entries (enrich_response = TRUE), une entree par ligne
v3_row <- function(id, seconds = 3600, user_id = 2L, project = "presta_x", client = "acme",
                   start = "2026-10-12T09:00:00+02:00", stop = "2026-10-12T10:00:00+02:00",
                   at_tz = "2026-10-12T10:00:00+02:00", tags = list(), task_id = NULL,
                   billable_cents = NULL, billable = FALSE, description = paste("entry", id)) {
  list(user_id = user_id, username = paste0("user", user_id), project_id = 1L,
       project_name = project, project_color = "0", project_hex = "#06aaf5",
       client_name = client, task_id = task_id, billable = billable, description = description,
       tag_ids = list(), tag_names = tags, billable_amount_in_cents = billable_cents,
       hourly_rate_in_cents = NULL, currency = "EUR",
       time_entries = list(list(id = id, seconds = seconds, start = start, stop = stop,
                                at = at_tz, at_tz = at_tz)),
       row_number = id)
}

# Faux Reports v3 : `pages` = liste de listes de lignes, servies dans l'ordre ;
# `totals` = secondes renvoyees par les appels successifs a /totals (la derniere
# valeur est repetee). Enregistre le corps de chaque requete.
serve_v3 <- function(pages, totals, server_dates = NULL) {
  bodies <- list(); n_search <- 0; n_totals <- 0
  send <- function(url, ..., body = NULL) {
    b <- if (is.null(body)) list() else jsonlite::fromJSON(body, simplifyVector = FALSE)
    bodies[[length(bodies) + 1]] <<- c(list(.url = url), b)
    if (grepl("/totals$", url)) {
      n_totals <<- n_totals + 1
      headers <- list()
      if (!is.null(server_dates)) {
        headers <- list(date = server_dates[min(n_totals, length(server_dates))])
      }
      return(fake_response(200, list(seconds = totals[min(n_totals, length(totals))], rates = list()),
                           headers = headers))
    }
    n_search <<- n_search + 1
    if (n_search > length(pages)) stop("unexpected extra call: ", url)
    page <- pages[[n_search]]
    nxt <- list()
    if (isTRUE(attr(page, "more"))) last <- page[[length(page)]]
    if (isTRUE(attr(page, "more"))) nxt <- list(
      `x-next-id` = as.character(last$time_entries[[1]]$id + 1),
      `x-next-row-number` = as.character(last$row_number + 1),
      `x-next-timestamp` = "1760000000")
    fake_response(200, page, headers = nxt)
  }
  list(send = send, bodies = function() bodies,
       searches = function() Filter(function(b) grepl("/search/time_entries$", b$.url), bodies),
       n_search = function() n_search, n_totals = function() n_totals)
}

# page de lignes v3 ; more = TRUE annonce une page suivante (en-tetes x-next-*)
v3_page <- function(ids, more = FALSE, ...) structure(lapply(ids, v3_row, ...), more = more)
