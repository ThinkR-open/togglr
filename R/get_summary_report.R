#' get summary report
#' 
#' by user then projet
#'
#' @param api_token the toggl api token
#' @param workspace_id the workspace id
#' @param since begin date
#' @param until stop date
#' @param user_agent user_agent
#' @param users users
#'
#' @importFrom lubridate %m-%
#' @export
#' @importFrom glue glue
#' @importFrom lubridate years
#' @importFrom  dplyr select one_of as_tibble
#' @importFrom stats setNames
#' @importFrom purrr map
#' @encoding UTF-8
get_summary_report <- function(api_token = get_toggl_api_token(),
                              workspace_id = get_workspace_id(api_token),
                              since = toggl_today() %m-% lubridate::years(1),
                              until = toggl_today(),
                              user_agent = toggl_user_agent(),
                              users = get_workspace_users(api_token=api_token, workspace_id=workspace_id)
                              ) {
  
  
  period <- as_toggl_period(since, until)
  since <- period$since
  until <- period$until
  url <- glue::glue("https://api.track.toggl.com/reports/api/v2/summary?workspace_id={workspace_id}&since={since}&until={until}&user_agent={utils::URLencode(user_agent, reserved = TRUE)}&grouping=users",
                    since=format(since, "%Y-%m-%d"),
                    until=format(until, "%Y-%m-%d")
                    
  )
  
  
  wp <- toggl_GET(url, api_token = api_token)$content
  
  
  togglr::simplify(wp$data, simplifyDataFrame = TRUE) -> out

  
  
  out %>%
    select(-total_currencies)%>% 
    as_tibble() %>%
    select(-title) %>% 
    left_join(users %>% select(id,fullname),by="id") %>% 
    select(id,user=fullname,time,items)
}
