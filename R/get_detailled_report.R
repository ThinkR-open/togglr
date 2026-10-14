#' One page of the detailled report (internal)
#'
#' @return a tibble of the page entries, with attributes `total_count`,
#'   `per_page` and `total_grand` as returned by the API
#' @importFrom glue glue
#' @importFrom lubridate years
#' @importFrom dplyr select left_join as_tibble
#' @importFrom stats setNames
#' @importFrom purrr map
#' @encoding UTF-8
#' @noRd
get_detailled_report_paged <- function(api_token = get_toggl_api_token(),
                                       workspace_id = get_workspace_id(api_token),
                                       since = toggl_today() %m-% lubridate::years(1),
                                       until = toggl_today(),
                                       page = 1,
                                       user_agent = toggl_user_agent()) {
  # since / until : des Date deja validees par get_detailled_report()
  url <- glue("https://api.track.toggl.com/reports/api/v2/details?workspace_id={workspace_id}&since={since}&until={until}&user_agent={utils::URLencode(user_agent, reserved = TRUE)}&grouping=users&page={page}",
              since = format(since, "%Y-%m-%dT00:00:00"),
              until = format(until, "%Y-%m-%dT00:00:00")
  )
  wp <- toggl_GET(url, api_token = api_token)$content

  if (is.null(wp$total_count) || is.null(wp$per_page)) {
    stop("unexpected answer from the toggl reports API (no total_count/per_page) for page ",
         page, " of ", format(since), " -> ", format(until), ".", call. = FALSE)
  }

  out <- normalize_detailled_report(as_tibble(togglr::simplify(wp$data, simplifyDataFrame = TRUE)))
  attr(out, "total_count") <- wp$total_count
  attr(out, "per_page") <- wp$per_page
  attr(out, "total_grand") <- wp$total_grand # somme des durees de la periode, NULL si absente
  out
}

#' Wait so that two requests to the API (reads, writes, retries) are at least
#' `min_interval` seconds apart (1 s is the documented rate limit; a larger
#' value spreads a long extraction to spare the hourly quota).
#'
#' Each call reserves the next free slot under the state lock, then sleeps until
#' it: with a shared state file (`togglr.state_file`), concurrent processes are
#' spaced too.
#'
#' With an `api_token`, the same lock pass first checks what is known of the
#' quota of the token in `scope`: an exhausted quota fails when its reset is
#' beyond `max_wait`, otherwise it is waited for; a spread announced by
#' [toggl_note_quota()] is waited for, at most `max_wait`. These waits only
#' concern this token: the shared slot is reserved after them.
#' @noRd
toggl_pace <- function(min_interval = getOption("togglr.min_interval", 1),
                       api_token = NULL, scope = "user", max_wait = Inf) {
  qkey <- if (!is.null(api_token)) toggl_quota_key(api_token, scope)
  skey <- if (!is.null(api_token)) toggl_quota_key(api_token, scope, "spread_until")
  done <- character(0)
  repeat {
    plan <- toggl_state_update(function(st) {
      now <- Sys.time()
      reset <- if (!is.null(qkey) && !"quota" %in% done) st[[qkey]]
      if (!is.null(reset) && reset > now) {
        wait <- ceiling(as.numeric(difftime(reset, now, units = "secs")))
        return(list(state = st, value = list(hold = wait, why = "quota")))
      }
      spread <- if (!is.null(skey) && !"spread" %in% done) st[[skey]]
      if (!is.null(spread) && spread > now) {
        return(list(state = st, value = list(
          hold = min(as.numeric(difftime(spread, now, units = "secs")), max_wait), why = "spread")))
      }
      # quota et etalement consommes par cet envoi
      if (!is.null(qkey)) {
        st[[qkey]] <- NULL
        st[[skey]] <- NULL
      }
      last <- st$last_report_call
      slot <- if (is.null(last)) now else max(now, last + min_interval)
      st$last_report_call <- slot
      list(state = st, value = list(slot = slot))
    })
    if (is.null(plan$hold)) break
    if (plan$why == "quota") {
      if (plan$hold > max_wait) {
        stop("toggl hourly API quota exhausted, resets in ", plan$hold, "s (> max_wait = ",
             max_wait, "s): no request sent, try again later.", call. = FALSE)
      }
      message("toggl hourly API quota exhausted, waiting ", plan$hold, "s for the reset")
    } else {
      message("toggl hourly API quota almost exhausted, spreading the requests: waiting ",
              ceiling(plan$hold), "s")
    }
    toggl_sleep(plan$hold)
    done <- c(done, plan$why)
  }
  wait <- as.numeric(difftime(plan$slot, Sys.time(), units = "secs"))
  if (wait > 0) toggl_sleep(wait)
}



#' Detailled report: every time entry of a period
#'
#' Downloads the time entries of the workspace between `since` and `until`
#' (both included) from the reports API v2, all pages included.
#'
#' @param api_token the toggl api token
#' @param workspace_id the workspace id
#' @param since begin date (included): a `Date`, a date-time or a text
#'   "AAAA-MM-JJ"; one year ago by default
#' @param until end date (included), same formats; today by default
#' @param user_agent user agent sent to the reports API
#' @return a tibble, one row per time entry, with fixed column types (see
#'   Details); 0 rows for an empty period
#' @export
#' @param max_page deprecated, ignored: the number of pages is now computed
#'   from the `total_count` returned by the API.
#' @param cache logical. If `TRUE`, a period fully fetched and checked is
#'   stored on disk and returned as is by later calls with the same `since` /
#'   `until`. Only use it for closed periods: entries created or edited later in
#'   Toggl will not be seen. Failures are never cached. Default `FALSE`.
#' @param cache_dir folder of the cache used when `cache = TRUE`: by default
#'   `options("togglr.cache_dir")`, or the `reports` sub-folder of
#'   `rappdirs::user_cache_dir("togglr")`
#' @param memoise_cache_dir deprecated, use `cache_dir`
#' @details Columns: `id`, `pid`, `tid`, `uid`, `description`, `start`, `end`,
#'   `updated` (text, ISO 8601), `dur` (milliseconds), `user`, `use_stop`,
#'   `client`, `project`, `project_color`, `project_hex_color`, `task`,
#'   `billable`, `is_billable`, `cur`, `tags` (list), then any column added by
#'   the API.
#'
#'   All the pages of the period are fetched, then the number of
#'   distinct entries is checked against the `total_count` announced by the
#'   API. If entries are added, removed or edited while paginating (detected by
#'   a change of `total_count` or of the total duration `total_grand` returned
#'   with each page), the period is fetched again, up to
#'   `options(togglr.report_attempts)` reads in all (3 by default); if the
#'   result is still incomplete, an error is raised.
#'   An HTTP error (quota, server error, authentication) raises an error too:
#'   the function never returns a silently truncated period.
#'
#'   Periods longer than a year (365 days or more between `since` and
#'   `until`, across a new year) are split by calendar year.
#' @importFrom lubridate dmy days year %m-%
get_detailled_report <- function(api_token = get_toggl_api_token(),
                                 workspace_id = get_workspace_id(api_token),
                                 since = toggl_today() %m-% lubridate::years(1),
                                 until = toggl_today(),
                                 user_agent = toggl_user_agent(),
                                 max_page = NULL,
                                 cache = FALSE,
                                 cache_dir = toggl_cache_dir(),
                                 memoise_cache_dir = NULL) {
  if (!is.null(memoise_cache_dir)) {
    warning("`memoise_cache_dir` is deprecated, use `cache_dir`.", call. = FALSE)
    cache_dir <- memoise_cache_dir
  }
  read_toggl_period()
  if (!is.null(max_page)) {
    warning("`max_page` is deprecated and ignored: the number of pages comes from ",
            "the total_count returned by the API.", call. = FALSE)
  }

  # Reports v2 refuse plus d'un an : au-dela de 365 jours d'ecart, decoupage par
  # annee civile (une annee civile, meme bissextile, part en une fois)
  if (year(since) != year(until) && difftime(until, since, units = "days") >= 365) {
    return(bind_rows(
      get_detailled_report(api_token = api_token, workspace_id = workspace_id,
                           since = dmy(glue("0101{year(until)}")), until = until,
                           user_agent = user_agent, cache = cache,
                           cache_dir = cache_dir),
      get_detailled_report(api_token = api_token, workspace_id = workspace_id,
                           since = since, until = dmy(glue("0101{year(until)}")) - days(1),
                           user_agent = user_agent, cache = cache,
                           cache_dir = cache_dir)
    ))
  }

  if (isTRUE(cache)) {
    cd <- cachem::cache_disk(dir = cache_dir)
    # par token : deux tokens d'un workspace ne voient pas forcement les memes entrees
    key <- paste("detailled", toggl_token_hash(api_token), workspace_id,
                 format(since, "%Y%m%d"), format(until, "%Y%m%d"), sep = "_")
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
                                   user_agent,
                                   attempts = getOption("togglr.report_attempts", 3)) {
  # un timer en cours fait grandir total_grand d'une page a l'autre si la
  # periode contient aujourd'hui : tolerer cette hausse, pas plus
  open <- until >= toggl_today()
  for (attempt in seq_len(attempts)) {
    started <- Sys.time()
    first <- get_detailled_report_paged(api_token = api_token, workspace_id = workspace_id,
                                        since = since, until = until,
                                        user_agent = user_agent, page = 1)
    total <- attr(first, "total_count")
    # Signature de la periode, renvoyee avec chaque page : une suppression suivie
    # d'une insertion laisse total_count inchange mais modifie la somme des durees
    grand <- attr(first, "total_grand")
    n_users <- max(1, length(unique(first$uid)))
    n_pages <- max(1, ceiling(total / attr(first, "per_page")))

    res <- list(first)
    moved <- FALSE
    for (page in seq_len(n_pages)[-1]) {
      res[[page]] <- get_detailled_report_paged(api_token = api_token, workspace_id = workspace_id,
                                                since = since, until = until,
                                                user_agent = user_agent, page = page)
      elapsed_ms <- 1000 * as.numeric(difftime(Sys.time(), started, units = "secs"))
      if (!identical(attr(res[[page]], "total_count"), total) ||
          !same_total_grand(attr(res[[page]], "total_grand"), grand,
                            growth_ms = if (open) (elapsed_ms + 1000) * n_users else 0)) {
        moved <- TRUE
        break
      }
    }

    out <- bind_rows(res)
    if (nrow(out) > 0) {
      # une entree vue deux fois (modifiee entre deux pages) : garder la version
      # la plus recente ; l'ordre d'origine est conserve sinon
      updated <- suppressWarnings(lubridate::ymd_hms(out$updated, quiet = TRUE))
      recent <- order(updated, decreasing = TRUE, method = "radix", na.last = TRUE)
      keep <- sort(recent[!duplicated(out$id[recent])])
      out <- out[keep, ]
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

#' Has the total duration of the period stayed the same between two pages?
#'
#' `growth_ms` is the increase allowed for running timers (their duration
#' grows with the clock); any decrease, or a larger increase, is an edit.
#' @noRd
same_total_grand <- function(now, before, growth_ms = 0) {
  if (is.null(now) || is.null(before)) return(is.null(now) && is.null(before))
  delta <- as.numeric(now) - as.numeric(before)
  delta >= 0 && delta <= growth_ms
}

#' Column types of the detailled report
#'
#' Fixed whatever the content of a page: a page where no entry has a task would
#' otherwise give a logical `task` column, that cannot be combined with a page
#' where `task` is text.
#' @noRd
detailled_report_types <- function() {
  list(id = numeric(0), pid = integer(0), tid = integer(0), uid = integer(0),
       description = character(0), start = character(0), end = character(0),
       updated = character(0), dur = numeric(0), user = character(0),
       use_stop = logical(0), client = character(0), project = character(0),
       project_color = character(0), project_hex_color = character(0),
       task = character(0), billable = numeric(0), is_billable = logical(0),
       cur = character(0), tags = list())
}

#' Give the known columns their fixed type (missing ones are added as NA);
#' columns unknown to togglr are kept as they are, after the known ones.
#' @noRd
normalize_detailled_report <- function(x) {
  types <- detailled_report_types()
  n <- nrow(x)
  known <- lapply(names(types), function(col) {
    v <- if (col %in% names(x)) x[[col]] else rep(NA, n)
    if (is.list(types[[col]])) {
      if (is.list(v)) v else as.list(v)
    } else {
      v[vapply(v, is.null, logical(1))] <- NA
      v <- unlist(v)
      switch(class(types[[col]]),
             numeric = as.numeric(v),
             integer = as.integer(v),
             logical = as.logical(v),
             character = as.character(v))
    }
  })
  names(known) <- names(types)
  extra <- x[setdiff(names(x), names(types))]
  dplyr::bind_cols(as_tibble(known), extra)
}

#' @noRd
empty_detailled_report <- function() {
  as_tibble(detailled_report_types())
}


#' Folder of the reports cache
#'
#' `options("togglr.cache_dir")`, or the `reports` sub-folder of
#' `rappdirs::user_cache_dir("togglr")` (kept apart from the files of the
#' memoise cache of togglr 0.2.x).
#' @noRd
toggl_cache_dir <- function() {
  getOption("togglr.cache_dir", file.path(rappdirs::user_cache_dir("togglr"), "reports"))
}

#' Empty the cache of the reports
#'
#' Removes the periods stored by `get_detailled_report(cache = TRUE)`.
#'
#' @param cache_dir folder of the cache
#' @export
#' @examples
#' \dontrun{
#' clean_cache()
#' }
clean_cache <- function(cache_dir = toggl_cache_dir()) {
  unlink(cache_dir, recursive = TRUE, force = TRUE)
}

#' clean memoise cache (deprecated)
#'
#' Deprecated: togglr no longer memoises. Removes the cache folder of togglr
#' 0.2.x (and the reports cache it contains). Use [clean_cache()].
#'
#' @param memoise_cache_dir folder to remove
#' @export
#' @examples
#' \dontrun{
#' clean_memoise_cache()
#' }
clean_memoise_cache <- function(memoise_cache_dir = getOption("togglr_memoise_dir", default = rappdirs::user_cache_dir("togglr"))) {
  warning("clean_memoise_cache() is deprecated: togglr no longer memoises, use clean_cache().",
          call. = FALSE)
  unlink(memoise_cache_dir, recursive = TRUE, force = TRUE)
}
