# Reports API v3 : resume (summary) et noms des utilisateurs, projets et clients

#' Groups of a Reports v3 summary
#' @noRd
toggl_report_summary <- function(workspace_id, api_token, since, until, grouping, sub_grouping,
                                 user_ids = NULL) {
  body <- list(start_date = format(since, "%Y-%m-%d"), end_date = format(until, "%Y-%m-%d"),
               grouping = grouping, sub_grouping = sub_grouping)
  if (length(user_ids)) body$user_ids <- as.list(user_ids)
  res <- toggl_report_post("/summary/time_entries", workspace_id, api_token, body)$content
  if (is.null(res$groups)) list() else res$groups
}

#' Sub-groups (time entries) of a summary group, in the format of Reports v2:
#' `title` (a data frame with `time_entry`), `time` (ms), `cur`, `sum`, `rate`
#' (not given by Reports v3: NA) and `local_start`
#' @noRd
summary_items <- function(sub_groups) {
  out <- data.frame(
    time = vapply(sub_groups, function(s) 1000 * as.numeric(null_to_na(s$seconds, 0)), 0),
    cur = rep(NA_character_, length(sub_groups)),
    sum = rep(NA_real_, length(sub_groups)),
    rate = rep(NA_real_, length(sub_groups)),
    local_start = sub("Z$", "", vapply(sub_groups, function(s) null_to_na(s$local_start, NA_character_), "")),
    stringsAsFactors = FALSE
  )
  out$title <- data.frame(
    time_entry = vapply(sub_groups, function(s) null_to_na(s$title, NA_character_), ""),
    stringsAsFactors = FALSE
  )
  out[, c("title", "time", "cur", "sum", "rate", "local_start")]
}

#' Names of the users, projects (with client and color) and clients of a
#' workspace, to label the ids returned by Reports v3
#' @noRd
toggl_report_names <- function(api_token, workspace_id, users = TRUE) {
  pick <- function(x, cols) {
    x <- as.data.frame(x)
    for (cn in setdiff(cols, names(x))) x[[cn]] <- rep(NA, nrow(x))
    x[, cols, drop = FALSE]
  }
  list(
    users = if (users) pick(get_workspace_users(api_token = api_token, workspace_id = workspace_id),
                           c("id", "fullname")),
    projects = pick(get_all_projects(api_token = api_token, workspace_id = workspace_id),
                    c("id", "name", "client_id", "color")),
    clients = pick(get_all_client_info(api_token = api_token, workspace_id = workspace_id),
                   c("id", "name"))
  )
}

#' @noRd
null_to_na <- function(x, na) if (is.null(x)) na else x
