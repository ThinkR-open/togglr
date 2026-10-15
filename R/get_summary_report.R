#' get summary report
#' 
#' by user then projet
#'
#' @param api_token the toggl api token
#' @param workspace_id the workspace id
#' @param since begin date
#' @param until stop date
#' @param user_agent ignored since the move to Reports v3 (the user agent is
#'   sent in the header of every request)
#' @param users the users of the workspace (a data frame with `id` and
#'   `fullname`), to name the rows
#'
#' @details The rounding set in the Toggl profile is not applied
#'   (`rounding = 0` in the request), as in Reports v2.
#' @importFrom lubridate %m-%
#' @return a tibble, one row per user: `id`, `user` (full name), `time` (ms) and `items` (a data frame of the user's time entries: `title`, `time` in ms, `local_start`; `cur`, `sum` and `rate` are `NA` since Reports v3)
#' @export
#' @importFrom glue glue
#' @importFrom lubridate years
#' @importFrom stats setNames
#' @encoding UTF-8
get_summary_report <- function(api_token = get_toggl_api_token(),
                              workspace_id = get_workspace_id(api_token),
                              since = toggl_today() %m-% lubridate::years(1),
                              until = toggl_today(),
                              user_agent = toggl_user_agent(),
                              users = get_workspace_users(api_token = api_token, workspace_id = workspace_id)) {
  read_toggl_period()
  groups <- toggl_report_summary(workspace_id = workspace_id, api_token = api_token,
                                 since = since, until = until,
                                 grouping = "users", sub_grouping = "time_entries")
  ids <- vapply(groups, function(g) null_to_na(g$id, NA_integer_), integer(1))
  dplyr::tibble(
    id = ids,
    user = users$fullname[match(ids, users$id)],
    time = vapply(groups, function(g) 1000 * sum(vapply(g$sub_groups, function(s) as.numeric(null_to_na(s$seconds, 0)), 0)), 0),
    items = lapply(groups, function(g) summary_items(g$sub_groups))
  )
}
