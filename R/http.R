#' Perform a GET request on the Toggl API and fail loudly
#'
#' Every read of the Toggl API goes through this function. Unlike a bare
#' `httr::GET()` + `content()`, an HTTP error is never turned into an empty
#' result, and the documented limits of the API are respected
#' (<https://engineering.toggl.com/docs/track/>):
#'
#' * 402 with quota headers means the hourly quota of the token is exhausted.
#'   The reset time is remembered per token and per quota scope (the workspace
#'   or organization of the url, or the user for `/me`, `/workspaces`; see
#'   `togglr.state_file`), and no request is sent in that scope before it: the
#'   call waits for the reset if it is within `max_wait`, otherwise it fails
#'   without contacting the API. A 402 without quota headers (a feature of a
#'   paid plan) stops at once.
#' * a few requests left in the scope are spread until the reset, each wait
#'   capped at `max_wait` (see `toggl_note_quota()`).
#' * 429 means the rate limit (about 1 request per second) is exceeded: the call
#'   backs off (`retry-after`, or 60 s, 120 s...) within `max_wait`.
#' * 5xx and network errors (timeout, DNS, connection reset) are retried with a
#'   short backoff. Every request times out after `getOption("togglr.timeout",
#'   120)` seconds, so that a stalled connection cannot block the session.
#' * any other non-200 status, or a retry budget exhausted, raises an error.
#'
#' @param url full url
#' @param api_token the toggl api token
#' @param max_tries maximum number of attempts
#' @param max_wait maximum number of seconds to wait before a single attempt.
#'   Longer waits (typically an exhausted hourly quota) fail immediately instead
#'   of blocking the session. Can be set with `options(togglr.max_wait = )`.
#'
#' @return a list with `content` (parsed body) and `headers`
#' @importFrom httr GET POST PUT PATCH DELETE authenticate content status_code headers http_status timeout user_agent
#' @noRd
toggl_GET <- function(url,
                      api_token = get_toggl_api_token(),
                      max_tries = 5,
                      max_wait = getOption("togglr.max_wait", 60)) {
  toggl_call("GET", url, api_token = api_token, max_tries = max_tries, max_wait = max_wait)
}

#' Perform a write request (POST, PUT, PATCH, DELETE) on the Toggl API
#'
#' Same policy as `toggl_GET()` (quota, 429, errors raised with the message of
#' the API), with one difference: a POST is not idempotent, so it is never
#' retried after a 5xx or a network error, as it may have been processed. A 429
#' is always retried: the request was refused, not processed.
#'
#' @param verb "POST", "PUT", "PATCH" or "DELETE"
#' @param body request body (a JSON string)
#' @param idempotent `TRUE` for a request that can safely be sent twice (PUT,
#'   PATCH, DELETE, and the read-only searches of the reports API): it is then
#'   retried after a 5xx or a network error
#' @inheritParams toggl_GET
#' @return a list with `content` (parsed body, `NULL` for an empty answer) and
#'   `headers`
#' @noRd
toggl_request <- function(verb, url,
                          api_token = get_toggl_api_token(),
                          body = NULL,
                          max_tries = 5,
                          max_wait = getOption("togglr.max_wait", 60),
                          idempotent = verb != "POST") {
  verb <- match.arg(verb, c("POST", "PUT", "PATCH", "DELETE"))
  toggl_call(verb, url, api_token = api_token, body = body,
             max_tries = max_tries, max_wait = max_wait, idempotent = idempotent)
}

#' @noRd
toggl_call <- function(verb, url, api_token, body = NULL, max_tries, max_wait,
                       idempotent = verb != "POST") {
  scope <- toggl_quota_scope(url)
  for (try in seq_len(max_tries)) {
    # quota connu, etalement et espacement : chaque envoi, relances comprises,
    # prend un creneau
    toggl_pace(api_token = api_token, scope = scope, max_wait = max_wait)
    resp <- tryCatch(
      if (verb == "GET") toggl_http_get(url, api_token) else toggl_http_send(verb, url, api_token, body),
      error = function(e) e
    )

    if (inherits(resp, "error")) {
      # timeout, DNS, connexion coupee... : traite comme un 5xx
      cause <- conditionMessage(resp)
      if (!idempotent) {
        stop("toggl API network error on ", verb, " ", sub("\\?.*$", "", url), ": ", cause,
             ". Not retried: the request may have been processed, check before sending it again.",
             call. = FALSE)
      }
      if (try == max_tries) {
        stop("toggl API network error on ", sub("\\?.*$", "", url), " after ", max_tries,
             " attempts: ", cause, call. = FALSE)
      }
      wait <- min(2^try, 30)
      if (wait > max_wait) {
        stop("toggl API network error on ", sub("\\?.*$", "", url), ": ", cause,
             ". Not retried: the retry would wait ", wait, "s (> max_wait = ", max_wait, "s).",
             call. = FALSE)
      }
      message("toggl API network error (", cause, "), retrying in ", wait, "s")
      toggl_sleep(wait)
      next
    }

    status <- status_code(resp)

    if (status >= 200 && status < 300) {
      # la requete est traitee : une erreur ici ne doit pas faire croire a un
      # echec (un POST relance cree un doublon)
      tryCatch(toggl_note_quota(resp, api_token, scope), error = function(e) {
        warning("togglr could not record the quota of the API (", conditionMessage(e),
                "); the request succeeded.", call. = FALSE)
      })
      body <- if (status == 204 || !length(resp$content)) NULL else tryCatch(content(resp), error = function(e) {
        if (verb == "GET") stop(e)
        warning("toggl API answer to ", verb, " ", sub("\\?.*$", "", url), " could not be read (",
                conditionMessage(e), "); the request succeeded.", call. = FALSE)
        NULL
      })
      return(list(content = body, headers = headers(resp)))
    }

    if (status == 402) {
      reset <- toggl_header_num(resp, "x-toggl-quota-resets-in")
      remaining <- toggl_header_num(resp, "x-toggl-quota-remaining")
      if (is.na(reset) && !identical(remaining, 0)) {
        # pas un quota : fonctionnalite reservee a un plan payant
        stop(toggl_error_message(resp, url), call. = FALSE)
      }
      toggl_state_set(toggl_quota_key(api_token, scope),
                      Sys.time() + (if (is.na(reset)) 3600 else max(1, reset)))
      if (try == max_tries) stop(toggl_error_message(resp, url), call. = FALSE)
      next # toggl_pace() waits for the reset, or fails, before any new request
    }

    retryable <- status == 429 || (status >= 500 && idempotent)
    if (!retryable || try == max_tries) {
      stop(toggl_error_message(resp, url), call. = FALSE)
    }

    wait <- toggl_retry_delay(resp, try)
    if (wait > max_wait) {
      stop(toggl_error_message(resp, url),
           " The API asks to wait ", wait, "s (> max_wait = ", max_wait,
           "s), try again later.", call. = FALSE)
    }
    message("toggl API returned ", status, ", retrying in ", wait, "s")
    toggl_sleep(wait)
  }
}

#' Short hash of a token, to key what is stored about it: the token itself is
#' never stored (the state and the cache live in files)
#' @noRd
toggl_token_hash <- function(api_token) {
  substr(as.character(openssl::sha256(as.character(api_token))), 1, 16)
}

#' State key of the quota of one token in one scope
#'
#' `what` is "quota_until" (reset of an exhausted quota) or "spread_until"
#' (next request allowed while few requests are left).
#' @noRd
toggl_quota_key <- function(api_token, scope = "user", what = "quota_until") {
  paste0(what, "_", toggl_token_hash(api_token), "_", scope)
}

#' Quota scope of a url
#'
#' Toggl counts the hourly quota per organization, and per user for the
#' endpoints outside any organization (`/me`, `/workspaces`). The organization
#' of a workspace is not known from the url, so the workspace stands for it: an
#' exhausted workspace does not block the other ones, nor `/me`. The API v9
#' says `workspaces/<id>`, Reports v3 says `workspace/<id>`: both are the same
#' scope.
#' @noRd
toggl_quota_scope <- function(url) {
  m <- regmatches(url, regexec("(workspaces?/|workspace_id=|organizations/)([0-9]+)", url))[[1]]
  if (!length(m)) {
    return("user")
  }
  paste0(if (m[2] == "organizations/") "org" else "ws", m[3])
}

#' @noRd
toggl_header_num <- function(resp, name) {
  x <- headers(resp)[[name]]
  suppressWarnings(as.numeric(if (is.null(x)) NA else x))
}

# Thin wrappers, so that tests can replace the network and the clock
#' @noRd
toggl_http_get <- function(url, api_token) {
  GET(url, authenticate(api_token, "api_token"), timeout(getOption("togglr.timeout", 120)),
      user_agent(toggl_user_agent()), encode = "json")
}

#' @noRd
toggl_http_send <- function(verb, url, api_token, body) {
  send <- switch(verb, POST = POST, PUT = PUT, PATCH = PATCH, DELETE = DELETE)
  send(url, authenticate(api_token, "api_token"), timeout(getOption("togglr.timeout", 120)),
       user_agent(toggl_user_agent()), encode = "json", body = body)
}

#' @noRd
toggl_sleep <- function(seconds) {
  Sys.sleep(seconds)
}

#' Seconds to wait before retrying a 429 or a 5xx
#' @noRd
toggl_retry_delay <- function(resp, try) {
  asked <- toggl_header_num(resp, "retry-after")
  if (!is.na(asked)) return(max(1, asked))
  if (status_code(resp) == 429) return(60 * try) # the docs ask to back off for minutes
  min(2^try, 30)
}

#' @noRd
toggl_error_message <- function(resp, url) {
  body <- tryCatch(content(resp, as = "text", encoding = "UTF-8"),
                   error = function(e) "")
  paste0(
    "toggl API error ", status_code(resp), " (", http_status(resp)$message, ") on ",
    sub("\\?.*$", "", url), # never print the query string (workspace, dates are enough in the call)
    if (nzchar(body)) paste0(": ", substr(body, 1, 200)) else "",
    "."
  )
}

#' Use the quota announced by a successful answer
#'
#' With `x-toggl-quota-remaining` and `x-toggl-quota-resets-in`: no request
#' left means no request before the reset (instead of a wasted 402); a few
#' requests left (fewer than `options(togglr.quota_spread)`, 10 by default) are
#' spread evenly until the reset: the next request of the token in this scope
#' waits `reset / remaining` seconds, at most `max_wait` (see `toggl_pace()`).
#' Other tokens and scopes are not slowed down.
#' @noRd
toggl_note_quota <- function(resp, api_token, scope = "user") {
  remaining <- toggl_header_num(resp, "x-toggl-quota-remaining")
  reset <- toggl_header_num(resp, "x-toggl-quota-resets-in")
  if (is.na(remaining) || is.na(reset)) return(invisible())
  if (remaining <= 0) {
    toggl_state_set(toggl_quota_key(api_token, scope), Sys.time() + max(1, reset))
  } else if (remaining < getOption("togglr.quota_spread", 10)) {
    toggl_state_set(toggl_quota_key(api_token, scope, "spread_until"),
                    Sys.time() + reset / remaining)
  }
  invisible()
}
