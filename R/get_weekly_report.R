#' get weekly report
#'
#' @param api_token the toggl api token
#' @param workspace_id the workspace id
#' @param since begin date
#' @param until stop date
#' @param user_agent user_agent
#'
#' @importFrom lubridate %m-%
#' @export
#' @importFrom lubridate years
#' @importFrom dplyr bind_rows mutate
#' @importFrom stats setNames
#' @importFrom purrr map
#' @importFrom glue glue
#' @encoding UTF-8
get_weekly_report <- function(api_token = get_toggl_api_token(),
                              workspace_id = get_workspace_id(api_token),
                              since = toggl_today() %m-% lubridate::years(1),
                              until = toggl_today(),
                              user_agent = toggl_user_agent()) {
  read_toggl_period()
  rows <- toggl_report_post("/weekly/time_entries", workspace_id, api_token,
                            list(start_date = format(since, "%Y-%m-%d"),
                                 end_date = format(until, "%Y-%m-%d")))$content
  names_ <- toggl_report_names(api_token, workspace_id)
  row_pid <- vapply(rows, function(r) null_to_na(r$project_id, NA_integer_), integer(1))
  row_uid <- vapply(rows, function(r) null_to_na(r$user_id, NA_integer_), integer(1))
  sec <- lapply(rows, function(r) vapply(r$seconds, function(x) as.numeric(null_to_na(x, 0)), 0))
  # (noms row_* : dplyr::tibble() verrait sinon ses propres colonnes pid / uid)
  # totaux en ms par jour puis total ; NA pour 0, comme Reports v2
  week_ms <- function(s) { v <- c(s, sum(s)) * 1000; v[v == 0] <- NA; v }
  projects <- unique(row_pid)
  pos <- match(projects, names_$projects$id)
  title <- data.frame(
    project = names_$projects$name[pos],
    client = names_$clients$name[match(names_$projects$client_id[pos], names_$clients$id)],
    color = "0",
    hex_color = names_$projects$color[pos],
    stringsAsFactors = FALSE
  )
  dplyr::tibble(
    pid = projects,
    title = title,
    totals = lapply(projects, function(p) week_ms(Reduce(`+`, sec[row_pid %in% p]))),
    details = lapply(projects, function(p) {
      k <- which(row_pid %in% p)
      d <- data.frame(uid = row_uid[k])
      d$title <- data.frame(user = names_$users$fullname[match(row_uid[k], names_$users$id)],
                            stringsAsFactors = FALSE)
      d$totals <- lapply(sec[k], week_ms)
      d
    })
  )
}
