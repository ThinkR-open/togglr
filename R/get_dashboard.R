#' recupere les données depuis toggl.com
#'
#' @param api_token the toggl api token
#' @param workspace_id the workspace id
#' @param since begin date
#' @param until stop date
#'
#' @export
#'
#' @importFrom lubridate years
#' @importFrom  dplyr select one_of
#' @importFrom stats setNames
#' @importFrom purrr map
#' @encoding UTF-8
get_dashboard <- function(api_token = get_toggl_api_token(),
                          workspace_id = get_workspace_id(),
                          since = Sys.Date() - lubridate::years(1),
                          until = Sys.Date()) {
  url <-
    sprintf(
      "https://toggl.com/reports/api/v2/summary?workspace_id=%s&since=%s&until=%s&user_agent=api_test",
      get_workspace_id(),
      format(since, "%Y-%m-%d"),
      format(until, "%Y-%m-%d")
      
    )
  wp <- content(GET(url,
                    # verbose(),
                    authenticate(api_token, "api_token"),
                    encode = "json"))
  
  
  jsonlite:::simplify(wp$data, simplifyDataFrame = TRUE) -> out
  
  
  synthese <-
    data.frame(id = out$id, out$title, time = out$time) %>%
  select(-one_of(c("color","hex_color")))
  
  tache <- out$items %>%
    setNames(synthese$project) %>%
    map( ~ {
      .x$title <-  .x$title$time_entry
      .x
    })
  
  res <- list(synthese = synthese,
              tache = tache)
  class(res) <- "toggl"
  res
}

n_to_tps <- function(n) {
  format(as.POSIXct(0, origin = Sys.Date(), tz = "GMT") + n / 1000
         ,
         "%H:%M:%S")
}



#' get project total time
#'
#' @param project_name project name
#' @param api_token the toggl api token
#' @param workspace_id the workspace id
#' @param since a date
#' @param until a date
#' @importFrom dplyr pull
#'
#' @export
#'
get_project_total <- function(project_name = get_context_project(),
                              api_token = get_toggl_api_token(),
                              workspace_id = get_workspace_id(),
                              since = Sys.Date() - lubridate::years(1),
                              until = Sys.Date()) {
  dash <-
    get_dashboard(
      api_token = api_token,
      workspace_id = workspace_id,
      since = since,
      until = until
    )
  dash$synthese  %>% filter(project == project_name) %>% pull("time") %>% n_to_tps
}

#' Get all project's names
#'
#' @param api_token the toggl api token
#' @param workspace_id the workspace id
#' @param since a date
#' @param until a date
#'
#' @export
#'
get_all_project_names <- function(api_token = get_toggl_api_token(),
                                  workspace_id = get_workspace_id(),
                                  since = Sys.Date() - lubridate::years(1),
                                  until = Sys.Date()) {
  dash <-
    get_dashboard(
      api_token = api_token,
      workspace_id = workspace_id,
      since = since,
      until = until
    )
  dash$synthese$project
}


#' get all client's name
#'
#' @param api_token the toggl api token
#' @param workspace_id the workspace id
#' @param since a date
#' @param until a date
#'
#' @export
#'
get_all_client_names <- function(api_token = get_toggl_api_token(),
                                 workspace_id = get_workspace_id(),
                                 since = Sys.Date() - lubridate::years(1),
                                 until = Sys.Date()) {
  dash <-
    get_dashboard(
      api_token = api_token,
      workspace_id = workspace_id,
      since = since,
      until = until
    )
  dash$synthese$client %>% unique()
}

#' get all task from a project
#'
#' @param project_name project name
#' @param api_token the toggl api token
#' @param workspace_id the workspace id
#' @param since a date
#' @param until a date
#' @param humain boolean humain readable time
#'
#' @export
#' @importFrom dplyr mutate
#'
get_project_task_detail <-
  function(project_name = get_context_project(),
           api_token = get_toggl_api_token(),
           workspace_id = get_workspace_id(),
           since = Sys.Date() - lubridate::years(1),
           until = Sys.Date(),
           humain = TRUE) {
    dash <-
      get_dashboard(
        api_token = api_token,
        workspace_id = workspace_id,
        since = since,
        until = until
      )
    out <- dash$tache[[project_name]]
    
    if (humain) {
      out <- out %>% mutate(time = n_to_tps(time))
    }
    out
  }
