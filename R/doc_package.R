#' togglr: client for the 'Toggl Track' API
#'
#' Track time spent in R with the toggl.com timesheet: read and write time
#' entries, projects and clients, and download reports (Reports API v3).
#'
#' @section Options:
#' Every request goes through the same policy (quota, rate limits, retries),
#' tuned by these options:
#' \describe{
#'   \item{`togglr.min_interval`}{minimal number of seconds between two
#'     requests (1). Raise it (e.g. 16) to spread a long extraction over the
#'     hourly quota of the API.}
#'   \item{`togglr.max_wait`}{maximal number of seconds a call waits for a
#'     retry or for the reset of an exhausted quota (60); beyond, it fails
#'     without sending the request.}
#'   \item{`togglr.timeout`}{timeout of a request, in seconds (120).}
#'   \item{`togglr.state_file`}{path of a file sharing the pacing of the
#'     requests and the known quota between R processes (callr, future,
#'     targets workers, cron jobs), under a lock. Without it, this state is
#'     private to the session.}
#'   \item{`togglr.quota_spread`}{when an answer announces fewer remaining
#'     requests than this (10), the next ones are spread until the reset of
#'     the quota.}
#'   \item{`togglr.page_size`}{number of entries per page of the detailled
#'     report (1000).}
#'   \item{`togglr.report_attempts`}{number of reads of a period that keeps
#'     changing during its pagination (3), before an error.}
#'   \item{`togglr.max_pages`}{maximal number of pages read by a paginated
#'     call (100), against an endless pagination.}
#'   \item{`togglr.cache_dir`}{folder of the reports cache
#'     (`get_detailled_report(cache = TRUE)`).}
#' }
#'
#' @section Quota:
#' Toggl counts an hourly quota per organization (endpoints of a workspace,
#' reports included) and per user (`/me`). togglr remembers an exhausted quota
#' per token and per scope; two workspaces of one organization each learn it
#' from their own 402 answer.
#'
#' @author vincent <vincent@@thinkr.fr>
"_PACKAGE"
