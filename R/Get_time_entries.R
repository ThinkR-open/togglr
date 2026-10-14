#' Get all time entries between 2 dates
#'
#' @param since begin date (One week ago by default)
#' @param until stop date (Now by defaut)
#' @param api_token the toggl api token
#' @param workspace_id the workspace id (to name the projects)
#'
#' @return a data.frame containing all time entries
#' @export
#' @importFrom lubridate years
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
  # RFC 3339 en UTC (`Z`) : un `+01:00` non encode serait lu comme une espace
  url <- glue::glue("https://api.track.toggl.com/api/v9/me/time_entries?start_date={format_toggl_utc(since)}&end_date={format_toggl_utc(until)}")
  res <- toggl_GET(url, api_token = api_token)$content

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
    select(
      start,
      stop,
      pretty_duration,
      duration,
      project_name,
      # description,
      pid,
      wid,
      everything()
    ) %>% arrange(desc(start))

}






