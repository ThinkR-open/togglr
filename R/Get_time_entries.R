#' Get all time entries between 2 dates
#'
#' @param since begin date (One week ago by default)
#' @param until stop date (Now by defaut)
#' @param api_token the toggl api token
#' @param workspace_id the workspace id (to name the projects and clients)
#'
#' @return a data.frame containing all time entries, with `project_name` and
#'   `client_name`. The endpoint goes back at most 3 months: for an older
#'   period, use [get_detailled_report()] with `user_ids`.
#' @export
#' @importFrom lubridate years %m-%
#' @importFrom dplyr select mutate case_when slice everything left_join arrange desc bind_rows %>% 
#' @importFrom stats setNames
#' @importFrom purrr map_df
#' @importFrom parsedate parse_iso_8601
#' @encoding UTF-8
#' @importFrom glue glue
#' @examples
#' \dontrun{
#' get_time_entries()
#' }
get_time_entries <- function(api_token = get_toggl_api_token(),
                             since = Sys.time() - lubridate::weeks(1),
                             until = Sys.time(),
                             workspace_id = get_workspace_id(api_token)) {
  # l'API v9 refuse un debut a plus de 3 mois : le dire avant toute requete
  limit <- toggl_today() %m-% lubridate::period(3, "months")
  if (as.Date(format(since, "%Y-%m-%d")) < limit) {
    stop("get_time_entries() reads /me/time_entries, which does not go back more than 3 months ",
         "(not before ", format(limit), "). For an older period, use ",
         "get_detailled_report(user_ids = <your user id>), which has no such limit.", call. = FALSE)
  }
  res <- me_time_entries(api_token, since, until)

  if (length(res) == 0) {
    return(data.frame())
  }


  
  
  
  # extract list of tags
  tags <- map(res, ~ .x$tags)

  woot <- get_project_id_and_name(api_token = api_token, workspace_id = workspace_id) %>% 
    mutate(id=as.character(id))
  
res %>% map(unlist) %>%  map(~ {
  avirer <- which(grepl(names(.x), pattern = "tag"))
    if (length(avirer) > 0){
      .x <- .x[-avirer]
    }
    return(.x)
  }) %>% bind_rows() %>%
  mutate(tags = tags) %>% 
    mutate(
      duration = as.numeric(duration),
      start = parse_iso_8601(start),
      duration = case_when(
        duration < 0 ~ as.integer(difftime(
          Sys.time(), start,
          units = "sec"
        )),
        TRUE ~ duration
      ),
      pretty_duration = prettyunits::pretty_sec(duration),
      stop = case_when(
        is.na(stop) ~ "0",
        TRUE ~ stop
      ),
      stop = parse_iso_8601(stop)
    ) %>%
    left_join(woot, by = c("project_id" = "id")) %>%
    left_join(client_names(api_token, workspace_id), by = c("project_id" = "project_id")) %>%
    select(
      start,
      stop,
      pretty_duration,
      duration,
      project_name,
      client_name,
      # description,
      pid,
      wid,
      everything()
    ) %>% arrange(desc(start))

}








#' Entries of /me/time_entries between two instants, never truncated
#'
#' The endpoint returns at most 1000 entries: a full answer is not trusted, the
#' period is split in two halves, read again, recursively.
#' @noRd
me_time_entries <- function(api_token, since, until, cap = 1000) {
  # RFC 3339 en UTC (`Z`) : un `+01:00` non encode serait lu comme une espace
  url <- glue::glue("https://api.track.toggl.com/api/v9/me/time_entries?start_date={format_toggl_utc(since)}&end_date={format_toggl_utc(until)}")
  res <- toggl_GET(url, api_token = api_token)$content
  if (length(res) < cap) return(res)
  middle <- as.POSIXct(since) + as.numeric(difftime(as.POSIXct(until), as.POSIXct(since), units = "secs")) / 2
  if (as.numeric(difftime(as.POSIXct(until), as.POSIXct(since), units = "secs")) < 2) {
    stop("more than ", cap, " toggl time entries in one second: cannot split further.", call. = FALSE)
  }
  message("toggl /me/time_entries returned ", cap, " entries (its maximum): splitting the period")
  out <- c(me_time_entries(api_token, since, middle, cap), me_time_entries(api_token, middle, until, cap))
  ids <- vapply(out, function(e) as.numeric(e$id), 0)
  out[!duplicated(ids)]
}

#' Client name of each project of a workspace
#' @noRd
client_names <- function(api_token, workspace_id) {
  p <- as.data.frame(get_all_projects(api_token = api_token, workspace_id = workspace_id))
  if (!"client_id" %in% names(p)) p$client_id <- rep(NA_integer_, nrow(p))
  cl <- as.data.frame(get_all_client_info(api_token = api_token, workspace_id = workspace_id))
  if (!all(c("id", "name") %in% names(cl))) cl <- data.frame(id = integer(0), name = character(0))
  out <- data.frame(project_id = as.character(p$id),
                    client_name = cl$name[match(p$client_id, cl$id)],
                    stringsAsFactors = FALSE)
  out[!duplicated(out$project_id), , drop = FALSE]
}
