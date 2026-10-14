#' Perform a GET request on the Toggl API and fail loudly
#'
#' Every read of the Toggl API goes through this function. Unlike a bare
#' `httr::GET()` + `content()`, an HTTP error is never turned into an empty
#' result, and the documented limits of the API are respected
#' (<https://engineering.toggl.com/docs/track/>):
#'
#' * 402 with quota headers means the hourly quota of the token is exhausted.
#'   The reset time is remembered (per token, see `togglr.state_file`), and no
#'   request is sent with that token before it: the call waits for the reset if
#'   it is within `max_wait`, otherwise it fails without contacting the API. A
#'   402 without quota headers (a feature of a paid plan) stops at once.
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
#' @importFrom httr GET authenticate content status_code headers http_status timeout
#' @noRd
toggl_GET <- function(url,
                      api_token = get_toggl_api_token(),
                      max_tries = 5,
                      max_wait = getOption("togglr.max_wait", 60)) {
  for (try in seq_len(max_tries)) {
    toggl_wait_quota(max_wait, api_token)
    resp <- tryCatch(toggl_http_get(url, api_token), error = function(e) e)

    if (inherits(resp, "error")) {
      # timeout, DNS, connexion coupee... : traite comme un 5xx
      cause <- conditionMessage(resp)
      if (try == max_tries) {
        stop("toggl API network error on ", sub("\\?.*$", "", url), " after ", max_tries,
             " attempts: ", cause, call. = FALSE)
      }
      wait <- min(2^try, 30)
      message("toggl API network error (", cause, "), retrying in ", wait, "s")
      toggl_sleep(wait)
      next
    }

    status <- status_code(resp)

    if (status == 200) {
      return(list(content = content(resp), headers = headers(resp)))
    }

    if (status == 402) {
      reset <- toggl_header_num(resp, "x-toggl-quota-resets-in")
      remaining <- toggl_header_num(resp, "x-toggl-quota-remaining")
      if (is.na(reset) && !identical(remaining, 0)) {
        # pas un quota : fonctionnalite reservee a un plan payant
        stop(toggl_error_message(resp, url), call. = FALSE)
      }
      toggl_state_set(toggl_quota_key(api_token),
                      Sys.time() + (if (is.na(reset)) 3600 else max(1, reset)))
      if (try == max_tries) stop(toggl_error_message(resp, url), call. = FALSE)
      next # toggl_wait_quota() waits for the reset, or fails, before any new request
    }

    if (!(status == 429 || status >= 500) || try == max_tries) {
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

#' Do not contact the API while the hourly quota is known to be exhausted
#' @noRd
toggl_wait_quota <- function(max_wait, api_token) {
  key <- toggl_quota_key(api_token)
  until <- toggl_state_get(key)
  if (is.null(until)) return(invisible())
  wait <- ceiling(as.numeric(difftime(until, Sys.time(), units = "secs")))
  if (wait <= 0) {
    toggl_state_set(key, NULL)
    return(invisible())
  }
  if (wait > max_wait) {
    stop("toggl hourly API quota exhausted, resets in ", wait, "s (> max_wait = ",
         max_wait, "s): no request sent, try again later.", call. = FALSE)
  }
  message("toggl hourly API quota exhausted, waiting ", wait, "s for the reset")
  toggl_sleep(wait)
  toggl_state_set(key, NULL)
}

#' State key of the quota of one token: a hash, the token itself is never
#' stored (the state may live in a file)
#' @noRd
toggl_quota_key <- function(api_token) {
  paste0("quota_until_", substr(as.character(openssl::sha256(as.character(api_token))), 1, 16))
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
      encode = "json")
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
