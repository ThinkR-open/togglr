#' get detailled report
#'
#' get detailled report by user then projet
#'
#' @param api_token the toggl api token
#' @param workspace_id the workspace id
#' @param since begin date (inclusive)
#' @param until stop date (inclusive)
#' @param page page
#' @param user_agent user_agent
#'
#' @return a tibble of the page entries, with attributes `total_count` and
#'   `per_page` as returned by the API
#' @import httr
#' @importFrom glue glue
#' @importFrom lubridate years
#' @importFrom dplyr select left_join as_tibble
#' @importFrom stats setNames
#' @importFrom purrr map
#' @encoding UTF-8
get_detailled_report_paged <- function(api_token = get_toggl_api_token(),
                                       workspace_id = get_workspace_id(api_token),
                                       since = Sys.Date() - lubridate::years(3),
                                       until = Sys.Date() + lubridate::days(1),
                                       page = 1,
                                       user_agent = "togglr") {
  url <- glue("https://api.track.toggl.com/reports/api/v2/details?workspace_id={workspace_id}&since={since}&until={until}&user_agent={user_agent}&grouping=users&page={page}",
              since = format(since, "%Y-%m-%dT00:00:00"),
              until = format(until, "%Y-%m-%dT00:00:00")
  )
  toggl_reports_pace()
  wp <- toggl_GET(url, api_token = api_token)$content

  if (is.null(wp$total_count) || is.null(wp$per_page)) {
    stop("unexpected answer from the toggl reports API (no total_count/per_page) for page ",
         page, " of ", format(since), " -> ", format(until), ".", call. = FALSE)
  }

  out <- as_tibble(togglr::simplify(wp$data, simplifyDataFrame = TRUE))
  attr(out, "total_count") <- wp$total_count
  attr(out, "per_page") <- wp$per_page
  out
}

#' Wait so that two calls to the reports API are at least `min_interval`
#' seconds apart (1 s is the documented rate limit of the reports API v2; a
#' larger value spreads a long extraction to spare the hourly quota)
#' @noRd
toggl_reports_pace <- function(min_interval = getOption("togglr.min_interval", 1)) {
  last <- .togglr_env$last_report_call
  if (!is.null(last)) {
    elapsed <- as.numeric(difftime(Sys.time(), last, units = "secs"))
    if (elapsed < min_interval) toggl_sleep(min_interval - elapsed)
  }
  .togglr_env$last_report_call <- Sys.time()
}

.togglr_env <- new.env(parent = emptyenv())


#' @export
#' @param max_page deprecated, ignored: the number of pages is now computed
#'   from the `total_count` returned by the API.
#' @param cache logical. If `TRUE`, a period fully fetched and checked is
#'   stored on disk and returned as is by later calls with the same `since` /
#'   `until`. Only use it for closed periods: entries created or edited later in
#'   Toggl will not be seen. Failures are never cached. Default `FALSE`.
#' @param memoise_cache_dir cache folder used when `cache = TRUE`, can be edited
#'   with `options('togglr_memoise_dir')` or `rappdirs::user_cache_dir("togglr")`
#'   by default
#' @details All the pages of the period are fetched, then the number of
#'   distinct entries is checked against the `total_count` announced by the
#'   API. If entries are added or removed while paginating, the period is
#'   fetched again once; if the result is still incomplete, an error is raised.
#'   An HTTP error (quota, server error, authentication) raises an error too:
#'   the function never returns a silently truncated period.
#'
#'   Periods longer than 366 days are split by calendar year.
#' @rdname get_detailled_report_paged
#' @importFrom lubridate dmy days year
get_detailled_report <- function(api_token = get_toggl_api_token(),
                                 workspace_id = get_workspace_id(api_token),
                                 since = Sys.Date() - lubridate::years(1),
                                 until = Sys.Date(),
                                 user_agent = "togglr",
                                 max_page = NULL,
                                 cache = FALSE,
                                 memoise_cache_dir = getOption("togglr_memoise_dir", default = rappdirs::user_cache_dir("togglr"))) {
  if (difftime(until, since, units = "days") > 366) {
    return(bind_rows(
      get_detailled_report(api_token = api_token, workspace_id = workspace_id,
                           since = dmy(glue("0101{year(until)}")), until = until,
                           user_agent = user_agent, cache = cache,
                           memoise_cache_dir = memoise_cache_dir),
      get_detailled_report(api_token = api_token, workspace_id = workspace_id,
                           since = since, until = dmy(glue("0101{year(until)}")) - days(1),
                           user_agent = user_agent, cache = cache,
                           memoise_cache_dir = memoise_cache_dir)
    ))
  }

  if (isTRUE(cache)) {
    cd <- cachem::cache_disk(dir = memoise_cache_dir)
    key <- paste("detailled", workspace_id, format(since, "%Y%m%d"),
                 format(until, "%Y%m%d"), sep = "_")
    cached <- cd$get(key)
    if (!cachem::is.key_missing(cached)) {
      return(cached)
    }
  }

  message("toggl detailled report ", format(since), " -> ", format(until))
  out <- fetch_detailled_period(api_token = api_token, workspace_id = workspace_id,
                                since = since, until = until, user_agent = user_agent)

  if (nrow(out) == 0) {
    out <- empty_detailled_report()
  }
  if (isTRUE(cache)) {
    cd$set(key, out)
  }
  out
}

#' Fetch every page of one period and check it is complete
#' @noRd
fetch_detailled_period <- function(api_token, workspace_id, since, until,
                                   user_agent, attempts = 2) {
  for (attempt in seq_len(attempts)) {
    first <- get_detailled_report_paged(api_token = api_token, workspace_id = workspace_id,
                                        since = since, until = until,
                                        user_agent = user_agent, page = 1)
    total <- attr(first, "total_count")
    n_pages <- max(1, ceiling(total / attr(first, "per_page")))

    res <- list(first)
    moved <- FALSE
    for (page in seq_len(n_pages)[-1]) {
      res[[page]] <- get_detailled_report_paged(api_token = api_token, workspace_id = workspace_id,
                                                since = since, until = until,
                                                user_agent = user_agent, page = page)
      if (!identical(attr(res[[page]], "total_count"), total)) {
        moved <- TRUE
        break
      }
    }

    out <- bind_rows(res)
    if (nrow(out) > 0) {
      out <- out[!duplicated(out$id), ]
    }
    if (!moved && nrow(out) == total) {
      return(out)
    }
    if (attempt < attempts) {
      message("toggl entries changed while paginating ", format(since), " -> ",
              format(until), ", fetching the period again")
    }
  }
  stop("incomplete toggl detailled report for ", format(since), " -> ", format(until),
       ": ", nrow(out), " distinct entries fetched, ", total, " announced by the API.",
       call. = FALSE)
}

#' @noRd
empty_detailled_report <- function() {
  structure(list(id = numeric(0), pid = integer(0), tid = logical(0),
                 uid = integer(0), description = character(0), start = character(0),
                 end = character(0), updated = character(0), dur = integer(0),
                 user = character(0), use_stop = logical(0), client = character(0),
                 project = character(0), project_color = character(0), project_hex_color = character(0),
                 task = logical(0), billable = numeric(0), is_billable = logical(0),
                 cur = character(0), tags = list()), row.names = integer(0),
            class = c("tbl_df", "tbl", "data.frame"))
}


#' clean memoise cache
#'
#' @rdname get_detailled_report_paged
#'
#' @export
#'
#' @examples
#' \dontrun{
#' clean_memoise_cache()
#' }
clean_memoise_cache <- function(memoise_cache_dir = getOption("togglr_memoise_dir", default = rappdirs::user_cache_dir("togglr"))) {
  unlink(memoise_cache_dir, recursive = TRUE, force = TRUE)
}
