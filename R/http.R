#' Perform a GET request on the Toggl API and fail loudly
#'
#' Every read of the Toggl API goes through this function. Unlike a bare
#' `httr::GET()` + `content()`, an HTTP error is never turned into an empty
#' result:
#'
#' * 429 (quota or rate limit) and 5xx are retried, waiting what the API asks
#'   for (`x-toggl-quota-resets-in` / `retry-after`), within `max_wait` seconds;
#' * any other non-200 status, or a retry budget exhausted, raises an error.
#'
#' @param url full url
#' @param api_token the toggl api token
#' @param max_tries maximum number of attempts
#' @param max_wait maximum number of seconds to wait for a single retry. A 429
#'   asking to wait longer (hourly quota exhausted) fails immediately instead of
#'   blocking the session. Can be set with `options(togglr.max_wait = )`.
#'
#' @return a list with `content` (parsed body) and `headers`
#' @importFrom httr GET authenticate content status_code headers http_status
#' @noRd
toggl_GET <- function(url,
                      api_token = get_toggl_api_token(),
                      max_tries = 5,
                      max_wait = getOption("togglr.max_wait", 60)) {
  for (try in seq_len(max_tries)) {
    resp <- toggl_http_get(url, api_token)
    status <- status_code(resp)

    if (status == 200) {
      return(list(content = content(resp), headers = headers(resp)))
    }

    if (!(status == 429 || status >= 500) || try == max_tries) {
      stop(toggl_error_message(resp, url), call. = FALSE)
    }

    wait <- toggl_retry_delay(resp, try)
    if (wait > max_wait) {
      stop(toggl_error_message(resp, url),
           " The API asks to wait ", wait, "s (> max_wait = ", max_wait,
           "s): quota exhausted, try again later.", call. = FALSE)
    }
    message("toggl API returned ", status, ", retrying in ", wait, "s")
    toggl_sleep(wait)
  }
}

# Thin wrappers, so that tests can replace the network and the clock
#' @noRd
toggl_http_get <- function(url, api_token) {
  GET(url, authenticate(api_token, "api_token"), encode = "json")
}

#' @noRd
toggl_sleep <- function(seconds) {
  Sys.sleep(seconds)
}

#' Seconds to wait before retrying a failed request
#' @noRd
toggl_retry_delay <- function(resp, try) {
  h <- headers(resp)
  num <- function(x) suppressWarnings(as.numeric(if (is.null(x)) NA else x))
  # x-toggl-quota-resets-in is sent with every answer: only relevant when the
  # hourly quota is actually exhausted, otherwise the 429 is a rate limit.
  if (isTRUE(num(h[["x-toggl-quota-remaining"]]) <= 0) &&
      !is.na(num(h[["x-toggl-quota-resets-in"]]))) {
    return(max(1, num(h[["x-toggl-quota-resets-in"]])))
  }
  if (!is.na(num(h[["retry-after"]]))) {
    return(max(1, num(h[["retry-after"]])))
  }
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
