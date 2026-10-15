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
#' `toggl_note_quota()` is waited for, at most `max_wait`. These waits only
#' concern this token: the shared slot is reserved after them.
#'
#' Returns, invisibly, the reserved slot (a `POSIXct`).
#' @noRd
toggl_pace <- function(min_interval = getOption("togglr.min_interval", 1),
                       api_token = NULL, scope = "user", max_wait = Inf) {
  qkey <- if (!is.null(api_token)) toggl_quota_key(api_token, scope)
  skey <- if (!is.null(api_token)) toggl_quota_key(api_token, scope, "spread_until")
  # le quota et l'etalement deja attendus : consommes, sauf s'ils ont change entre-temps
  waited_for <- NULL
  spread_waited <- NULL
  repeat {
    plan <- toggl_state_update(function(st) {
      now <- Sys.time()
      reset <- if (!is.null(qkey)) st[[qkey]]
      if (!is.null(reset) && identical(reset, waited_for)) {
        reset <- NULL
      }
      if (!is.null(reset) && reset > now) {
        wait <- ceiling(as.numeric(difftime(reset, now, units = "secs")))
        return(list(state = st, value = list(hold = wait, why = "quota", reset = reset)))
      }
      spread <- if (!is.null(skey)) st[[skey]]
      if (!is.null(spread) && identical(spread, spread_waited)) {
        spread <- NULL
      }
      if (!is.null(spread) && spread > now) {
        return(list(state = st, value = list(
          hold = min(as.numeric(difftime(spread, now, units = "secs")), max_wait),
          why = "spread", spread = spread)))
      }
      # quota attendu (ou echu) et etalement attendu (ou echu) consommes par cet
      # envoi ; une cle posee par un autre processus pendant l'attente n'est
      # jamais effacee ici
      if (!is.null(qkey)) {
        current <- st[[qkey]]
        if (is.null(current) || current <= now || identical(current, waited_for)) {
          st[[qkey]] <- NULL
        }
        current <- st[[skey]]
        if (is.null(current) || current <= now || identical(current, spread_waited)) {
          st[[skey]] <- NULL
        }
      }
      last <- st$last_report_call
      slot <- if (is.null(last)) now else max(now, last + min_interval)
      st$last_report_call <- slot
      list(state = st, value = list(slot = slot))
    })
    if (is.null(plan$hold)) {
      break
    }
    if (plan$why == "quota") {
      if (plan$hold > max_wait) {
        stop("toggl hourly API quota exhausted, resets in ", plan$hold, "s (> max_wait = ",
             max_wait, "s): no request sent, try again later.", call. = FALSE)
      }
      message("toggl hourly API quota exhausted, waiting ", plan$hold, "s for the reset")
      waited_for <- plan$reset
    } else {
      message("toggl hourly API quota almost exhausted, spreading the requests: waiting ",
              ceiling(plan$hold), "s")
      spread_waited <- plan$spread
    }
    toggl_sleep(plan$hold)
  }
  wait <- as.numeric(difftime(plan$slot, Sys.time(), units = "secs"))
  if (wait > 0) {
    toggl_sleep(wait)
  }
  invisible(plan$slot)
}



#' Detailled report: every time entry of a period
#'
#' Downloads the time entries of the workspace between `since` and `until`
#' (both included) from the reports API v3, all pages included.
#'
#' @param api_token the toggl api token
#' @param workspace_id the workspace id
#' @param since begin date (included): a `Date`, a date-time or a text
#'   "AAAA-MM-JJ"; one year ago by default
#' @param until end date (included), same formats; today by default
#' @param user_agent ignored since the move to Reports v3 (the user agent is
#'   sent in the header of every request)
#' @param user_ids optional, ids of the users whose entries are wanted (all
#'   the users of the workspace by default)
#' @return a tibble, one row per time entry, with fixed column types (see
#'   Details); 0 rows for an empty period
#' @export
#' @param max_page deprecated, ignored: every page is read.
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
#'   The entries come from Reports v3 (`/search/time_entries`, enriched with
#'   the names of users, projects, clients and tags), with large pages
#'   (`options(togglr.page_size)`, 1000 by default).
#'   The period is checked against the total duration announced by the API
#'   before and after the pagination, and against the edit time of every entry
#'   received; if entries were added, removed or edited meanwhile, the period
#'   is fetched again, up to `options(togglr.report_attempts)` reads in all (3
#'   by default), then an error is raised. One movement escapes these checks:
#'   an entry deleted and another one of exactly the same duration created
#'   during the read, when the new entry is not in the pages received. The
#'   rounding set in the Toggl profile is not applied. A running entry is left out. An HTTP error (quota, server
#'   error, authentication) raises an error too: the function never returns a
#'   silently truncated period.
#'
#'   Periods longer than a year (365 days or more between `since` and
#'   `until`, across a new year) are split by calendar year.
#' @importFrom lubridate dmy days year %m-%
#' @importFrom dplyr as_tibble bind_rows
get_detailled_report <- function(api_token = get_toggl_api_token(),
                                 workspace_id = get_workspace_id(api_token),
                                 since = toggl_today() %m-% lubridate::years(1),
                                 until = toggl_today(),
                                 user_agent = toggl_user_agent(),
                                 max_page = NULL,
                                 cache = FALSE,
                                 cache_dir = toggl_cache_dir(),
                                 memoise_cache_dir = NULL,
                                 user_ids = NULL) {
  if (!is.null(memoise_cache_dir)) {
    warning("`memoise_cache_dir` is deprecated, use `cache_dir`.", call. = FALSE)
    cache_dir <- memoise_cache_dir
  }
  read_toggl_period()
  if (!is.null(max_page)) {
    warning("`max_page` is deprecated and ignored: every page is read.", call. = FALSE)
  }

  # Reports v3 refuse plus de 366 jours : au-dela de 365 jours d'ecart, decoupage par
  # annee civile (une annee civile, meme bissextile, part en une fois)
  if (year(since) != year(until) && difftime(until, since, units = "days") >= 365) {
    return(bind_rows(
      get_detailled_report(api_token = api_token, workspace_id = workspace_id,
                           since = dmy(glue("0101{year(until)}")), until = until,
                           user_agent = user_agent, cache = cache,
                           cache_dir = cache_dir, user_ids = user_ids),
      get_detailled_report(api_token = api_token, workspace_id = workspace_id,
                           since = since, until = dmy(glue("0101{year(until)}")) - days(1),
                           user_agent = user_agent, cache = cache,
                           cache_dir = cache_dir, user_ids = user_ids)
    ))
  }

  if (isTRUE(cache)) {
    cd <- cachem::cache_disk(dir = cache_dir)
    # par token : deux tokens d'un workspace ne voient pas forcement les memes entrees
    key <- paste("detailled_v3", toggl_token_hash(api_token), workspace_id,
                 format(since, "%Y%m%d"), format(until, "%Y%m%d"),
                 paste(sort(user_ids), collapse = "-"), sep = "_")
    cached <- cd$get(key)
    if (!cachem::is.key_missing(cached)) {
      return(cached)
    }
  }

  message("toggl detailled report ", format(since), " -> ", format(until))
  out <- fetch_detailled_period(api_token = api_token, workspace_id = workspace_id,
                                since = since, until = until, user_ids = user_ids)

  if (nrow(out) == 0) {
    out <- empty_detailled_report()
  }
  if (isTRUE(cache)) {
    cd$set(key, out)
  }
  out
}

#' Fetch one period from Reports v3 and check it is complete
#'
#' `/search/time_entries` is paginated with `X-Next-Row-Number` (sent back as
#' `first_row_number`). The period is complete when the total duration announced by
#' `/search/time_entries/totals` is the same before and after the pagination
#' and equals the sum of the entries received, and when no entry received was
#' edited after the read started (the pages are numbered by row: an entry
#' deleted and another one inserted during the pagination shift the rows, and
#' equal durations leave the totals unchanged). An entry deleted and replaced
#' by one of the same duration that is never received is not detected. A
#' running entry (no `stop`) is left out, as in Reports v2; its growth between
#' the two totals is tolerated, and only its.
#' @noRd
fetch_detailled_period <- function(api_token, workspace_id, since, until, user_ids = NULL,
                                   attempts = getOption("togglr.report_attempts", 3)) {
  base <- list(start_date = format(since, "%Y-%m-%d"), end_date = format(until, "%Y-%m-%d"),
               rounding = 0L)
  if (length(user_ids)) {
    base$user_ids <- as.list(user_ids)
  }
  max_pages <- getOption("togglr.max_pages", 100)

  for (attempt in seq_len(attempts)) {
    started <- toggl_now()
    before <- toggl_report_totals(workspace_id = workspace_id, api_token = api_token, base = base)
    rows <- list()
    cursor <- list()
    for (page in seq_len(max_pages + 1)) {
      if (page > max_pages) {
        stop("more than ", max_pages, " pages in the toggl detailled report for ", format(since),
             " -> ", format(until), " (option togglr.max_pages): stopping.", call. = FALSE)
      }
      body <- c(base, list(page_size = as.integer(getOption("togglr.page_size", 1000)),
                           enrich_response = TRUE), cursor)
      resp <- toggl_report_post("/search/time_entries", workspace_id = workspace_id,
                                api_token = api_token, body = body)
      rows <- c(rows, resp$content)
      # page suivante par son numero de ligne : avec first_id / first_timestamp,
      # l'API renvoie une page vide ; un mouvement pendant la pagination est
      # detecte par les totaux et par les dates de modification
      next_row <- resp$headers[["x-next-row-number"]]
      if (is.null(next_row) || !nzchar(next_row)) {
        break
      }
      cursor <- list(first_row_number = as.numeric(next_row))
    }
    after <- toggl_report_totals(workspace_id = workspace_id, api_token = api_token, base = base)

    entries <- v3_rows_to_entries(rows)
    running <- is.na(entries$end)
    out <- entries[!running, ]
    received <- sum(out$dur) / 1000
    running_s <- sum(entries$dur[running]) / 1000
    updated <- suppressWarnings(lubridate::ymd_hms(entries$updated, quiet = TRUE))
    edited <- sum(!is.na(updated) & updated > started)
    elapsed <- as.numeric(difftime(toggl_now(), started, units = "secs"))
    # un timer en cours grandit avec l'horloge : tolerer sa seule croissance
    growth <- if (any(running)) (elapsed + 1) * sum(running) else 0
    stable <- after >= before && after - before <= growth
    # les totaux arrondissent la somme exacte (ms), la recherche arrondit chaque
    # entree : jusqu'a 1 s d'ecart par entree recue (une entree ou une page
    # manquante pese bien plus) ; un mouvement pendant la lecture est detecte par
    # l'egalite stricte des totaux avant / apres
    rounding <- nrow(entries)
    # totaux avec ou sans le temps des entrees en cours : les deux sont acceptes
    complete <- abs(after - received - running_s) <= growth + rounding ||
      (any(running) && abs(after - received) <= growth + rounding)
    if (stable && complete && edited == 0) {
      return(out)
    }
    if (attempt < attempts) {
      message("toggl entries changed while paginating ", format(since), " -> ",
              format(until), ", fetching the period again")
    }
  }
  if (edited > 0) {
    stop("incomplete toggl detailled report for ", format(since), " -> ", format(until),
         ": ", edited, " entries edited during the read, after ", attempts, " reads.",
         call. = FALSE)
  }
  stop("incomplete toggl detailled report for ", format(since), " -> ", format(until),
       ": ", received, " s of entries fetched, ", after, " s announced by the API",
       " (", before, " s before the pagination).", call. = FALSE)
}

#' URL of a Reports v3 endpoint of a workspace
#' @noRd
toggl_reports_v3_url <- function(workspace_id, path) {
  paste0("https://api.track.toggl.com/reports/api/v3/workspace/", workspace_id, path)
}

#' POST a read-only search to Reports v3 (retried like a GET)
#' @noRd
toggl_report_post <- function(path, workspace_id, api_token, body) {
  toggl_request("POST", toggl_reports_v3_url(workspace_id, path), api_token = api_token,
                body = toJSON(body, auto_unbox = TRUE, digits = NA), idempotent = TRUE)
}

#' Total duration (seconds) of a period, from Reports v3
#' @noRd
toggl_report_totals <- function(workspace_id, api_token, base) {
  res <- toggl_report_post("/search/time_entries/totals", workspace_id = workspace_id,
                           api_token = api_token, body = base)$content
  if (is.null(res$seconds)) {
    stop("unexpected answer from the toggl reports API: no total duration for ",
         base$start_date, " -> ", base$end_date, ".", call. = FALSE)
  }
  as.numeric(res$seconds)
}

#' Rows of /search/time_entries (enriched) to the columns of the detailled report
#'
#' One output row per time entry; an entry seen twice keeps its most recently
#' updated version, in the original order otherwise.
#' @noRd
v3_rows_to_entries <- function(rows) {
  null_na <- function(x, na = NA) {
    if (is.null(x)) {
      return(na)
    }
    x
  }
  entries <- lapply(rows, function(r) {
    lapply(r$time_entries, function(te) {
      list(id = te$id, pid = null_na(r$project_id), tid = null_na(r$task_id),
           uid = null_na(r$user_id), description = null_na(r$description, NA_character_),
           start = null_na(te$start, NA_character_), end = null_na(te$stop, NA_character_),
           updated = null_na(te$at_tz, null_na(te$at, NA_character_)),
           dur = as.numeric(null_na(te$seconds, NA)) * 1000,
           user = null_na(r$username, NA_character_), use_stop = FALSE,
           client = null_na(r$client_name, NA_character_),
           project = null_na(r$project_name, NA_character_),
           project_color = null_na(r$project_color, "0"), # "0" sans projet, comme en v2
           project_hex_color = null_na(r$project_hex, NA_character_),
           task = null_na(r$task_name, NA_character_),
           billable = if (is.null(r$billable_amount_in_cents)) 0 else r$billable_amount_in_cents / 100,
           is_billable = null_na(r$billable), cur = null_na(r$currency, NA_character_),
           tags = list(as.character(unlist(r$tag_names))))
    })
  })
  entries <- unlist(entries, recursive = FALSE)
  if (!length(entries)) {
    return(empty_detailled_report())
  }
  cols <- names(detailled_report_types())
  out <- lapply(cols, function(cn) {
    v <- lapply(entries, `[[`, cn)
    if (cn == "tags") lapply(v, `[[`, 1) else unlist(v)
  })
  names(out) <- cols
  out <- normalize_detailled_report(as_tibble(out))
  updated <- suppressWarnings(lubridate::ymd_hms(out$updated, quiet = TRUE))
  recent <- order(updated, decreasing = TRUE, method = "radix", na.last = TRUE)
  out[sort(recent[!duplicated(out$id[recent])]), ]
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
#' @return invisibly, the result of [unlink()] (0 for success)
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
#' @return invisibly, the result of [unlink()] (0 for success)
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
