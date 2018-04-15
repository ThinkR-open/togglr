#' Get all time entries between 2 dates
#'
#' @param since begin date (One week ago by default)
#' @param until stop date (Now by defaut)
#' @param api_token the toggl api token
#'
#' @return a data.frame containing all time entries
#' @export
#' @importFrom lubridate years
#' @importFrom  dplyr select mutate case_when slice everything left_join
#' @importFrom stats setNames
#' @importFrom purrr map_df
#' @importFrom parsedate format_iso_8601 parse_iso_8601
#' @encoding UTF-8
#' @examples
#' \dontrun{
#' get_time_entries()
#' }
get_time_entries <- function(api_token = get_toggl_api_token(),
                             since = Sys.time() - lubridate::weeks(1),
                             until = Sys.time()){
  url <- glue::glue("https://www.toggl.com/api/v8/time_entries?start_date={format_iso_8601(since)}&end_date={format_iso_8601(until)}")
  res <- content(GET(url,
              # verbose(),
              authenticate(api_token, "api_token"),
              encode = "json"))
  
  if (length(res)==0) {return(data.frame())}
  
  res %>%
    map_df(invisible) %>%
    mutate(
      start = parse_iso_8601(start)  ,
      duration = case_when(duration < 0 ~ as.integer(difftime(
        Sys.time(), start, units = "sec"
      )),
      TRUE ~ duration)
      ,
      pretty_duration = prettyunits::pretty_sec(duration)
      ,
      stop = case_when(is.na(stop) ~ "0",
                       TRUE ~ stop),
      stop = parse_iso_8601(stop)
    ) %>% 
    left_join(get_project_id_and_name(),by= c("pid"="id"))%>%
    select(start,
           stop,
           pretty_duration,
           duration,
           project_name,
           description,
           pid,
           wid,
           everything()) %>%
    slice(nrow(.):1) 
  
}