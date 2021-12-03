#' get weekly report
#'
#' @param api_token the toggl api token
#' @param workspace_id the workspace id
#' @param since begin date
#' @param until stop date
#' @param user_agent user_agent
#'
#' @export
#' @import httr
#' @importFrom lubridate years
#' @import dplyr
#' @importFrom stats setNames
#' @importFrom purrr map
#' @importFrom glue glue
#' @encoding UTF-8
get_weekly_report <- function(api_token = get_toggl_api_token(),
                          workspace_id = get_workspace_id(api_token),
                          since = Sys.Date() - lubridate::years(1),
                          until = Sys.Date(),
                          user_agent="togglr") {
  
  
  url <- glue::glue("https://api.track.toggl.com/reports/api/v2/weekly?workspace_id={workspace_id}&since={since}&until={until}&user_agent={user_agent}",
                    
                    since=format(since, "%Y-%m-%d"),
                    until=format(until, "%Y-%m-%d")
                    
                    )
  

  wp <- content(GET(url,
                    # verbose(),
                    authenticate(api_token, "api_token"),
                    encode = "json"))
  
  
  # 
  # wp$total_grand
  # wp$total_billable
  # wp$total_currencies
  # wp$week_totals
  # wp$data
  
  # jsonlite:::simplify(wp$data, simplifyDataFrame = TRUE) -> out
  togglr::simplify(wp$data, simplifyDataFrame = TRUE) -> out
  # 
  # 
  # synthese <-
  #   data.frame(id = out$id, out$title, time = out$time) %>%
  #   select(-one_of(c("color","hex_color")))
  # 
  # tache <- out$items %>%
  #   setNames(synthese$project) %>%
  #   map( ~ {
  #     .x$title <-  .x$title$time_entry
  #     .x
  #   })
  # 
  # 
  # res <- list(synthese = synthese,
  #             tache = tache)
  # class(res) <- "toggl"
  out%>% as_tibble()
}
