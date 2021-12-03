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
#' @export
#' @importFrom glue glue
#' @import httr
#' @importFrom lubridate years
#' @importFrom  dplyr select one_of as_tibble
#' @importFrom stats setNames
#' @importFrom purrr map
#' @encoding UTF-8
get_summary_report <- function(api_token = get_toggl_api_token(),
                              workspace_id = get_workspace_id(api_token),
                              since = Sys.Date() - lubridate::years(1),
                              until = Sys.Date(),
                              user_agent="togglr",
                              users = get_workspace_users(api_token=api_token, workspace_id=workspace_id)
                              ) {
  
  
  url <- glue::glue("https://api.track.toggl.com/reports/api/v2/summary?workspace_id={workspace_id}&since={since}&until={until}&user_agent={user_agent}&grouping=users",
                    since=format(since, "%Y-%m-%d"),
                    until=format(until, "%Y-%m-%d")
                    
  )
  
  
  wp <- content(GET(url,
                    # verbose(),
                    authenticate(api_token, "api_token"),
                    encode = "json"))
  
  
  togglr::simplify(wp$data, simplifyDataFrame = TRUE) -> out

  
  
  out %>%
    select(-total_currencies)%>% 
    as_tibble() %>%
    select(-title) %>% 
    left_join(users %>% select(id,fullname),by="id") %>% 
    select(id,user=fullname,time,items)
}
