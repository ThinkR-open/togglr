#' get all data from <https://toggl.com>
#'
#' @param api_token the toggl api token
#' @param workspace_id the workspace id
#' @param since begin date
#' @param user_ids optional, ids of the users whose time is counted (all by
#'   default)
#' @param until stop date
#'
#' @importFrom lubridate %m-%
#' @return an object of class `toggl`: a list with `synthese` (one row per project: `id`, `project`, `client`, `time` in ms) and `tache` (per project, a data frame of its time entries: `title`, `time` in ms, `local_start`)
#' @export
#' @importFrom lubridate years
#' @importFrom stats setNames
#' @encoding UTF-8
get_dashboard <- function(api_token = get_toggl_api_token(),
                          workspace_id = get_workspace_id(api_token),
                          since = toggl_today() %m-% lubridate::years(1),
                          until = toggl_today(),
                          user_ids = NULL) {
  read_toggl_period()
  groups <- toggl_report_summary(workspace_id = workspace_id, api_token = api_token,
                                 since = since, until = until,
                                 grouping = "projects", sub_grouping = "time_entries",
                                 user_ids = user_ids)
  names_ <- toggl_report_names(api_token = api_token, workspace_id = workspace_id, users = FALSE)
  ids <- vapply(groups, function(g) null_to_na(g$id, NA_integer_), integer(1))
  pos <- match(ids, names_$projects$id)
  synthese <- data.frame(
    id = ids,
    project = names_$projects$name[pos],
    client = names_$clients$name[match(names_$projects$client_id[pos], names_$clients$id)],
    time = vapply(groups, function(g) 1000 * sum(vapply(g$sub_groups, function(s) as.numeric(null_to_na(s$seconds, 0)), 0)), 0),
    stringsAsFactors = FALSE
  )
  tache <- lapply(groups, function(g) {
    it <- summary_items(g$sub_groups)
    it$title <- it$title$time_entry
    it
  })
  names(tache) <- synthese$project
  res <- list(synthese = synthese, tache = tache)
  class(res) <- "toggl"
  res
}



#' @noRd
#' @importFrom prettyunits pretty_ms
n_to_tps <- function(n) {
  # format(as.POSIXct(0, origin = Sys.Date(), tz = "GMT") + n / 1000, "%H:%M:%S")
  # 
  #   difftime(as.POSIXct(0, origin = Sys.Date(), tz = "GMT") + n / 1000,Sys.Date(),units = "hour") 
  #   
  #   
  # library(lubridate)  
  # new_duration()  
  # duration(104046451/1000,units = "seconds") %>% format("%H:%M:%S")
  prettyunits::pretty_ms(n)
}




#' get project total time
#'
#' @param project_name project name
#' @param api_token the toggl api token
#' @param workspace_id the workspace id
#' @param since a date
#' @param until a date
#' @details The rounding set in the Toggl profile is not applied
#'   (`rounding = 0` in the request), as in Reports v2.
#' @importFrom dplyr pull
#'
#' @return the total time spent on the project, as text (e.g. "12:30:00"), the running entry included
#' @export
#'
get_project_total <- function(project_name = get_context_project(),
                              
                              api_token = get_toggl_api_token(),
                              workspace_id = get_workspace_id(api_token),
                              since = toggl_today() %m-% lubridate::years(1),
                              until = toggl_today()) {
  dash <-
    get_dashboard(
      api_token = api_token,
      workspace_id = workspace_id,
      since = since,
      until = until
    )
  dash$synthese  %>% filter(project == project_name) %>% pull("time") ->tmps
  
  (get_current_duration(api_token = api_token) + tmps)  %>% as.numeric() %>% n_to_tps
  
}

#' Get all project's names
#'
#' @param api_token the toggl api token
#' @param workspace_id the workspace id
#' @param since a date
#' @param user_ids optional, only the projects these users worked on
#' @param until a date
#'
#' @return a character vector of the project names
#' @export
#'

get_all_project_names <- function(api_token = get_toggl_api_token(),
                                  workspace_id = get_workspace_id(api_token),
                                  since = toggl_today() %m-% lubridate::years(1),
                                  until = toggl_today(),
                                  user_ids = NULL) {
  dash <-
    get_dashboard(
      api_token = api_token,
      workspace_id = workspace_id,
      since = since,
      until = until,
      user_ids = user_ids
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

#' @return a character vector of the client names, without duplicates
#' @export
#'
get_all_client_names <- function(api_token = get_toggl_api_token(),
                                 workspace_id = get_workspace_id(api_token),
                                 since = toggl_today() %m-% lubridate::years(1),
                                 until = toggl_today()) {
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

#' @return a data frame of the tasks of the project (`title`, `time`...), the running entry included
#' @export
#' @importFrom dplyr mutate case_when full_join tribble
#' @importFrom lubridate years
#'
#'
get_current_project_task_detail <-
  function(project_name = get_context_project(),
           api_token = get_toggl_api_token(),
           workspace_id = get_workspace_id(api_token),
           since = toggl_today() %m-% lubridate::years(1),
           until = toggl_today(),
           humain = TRUE) {
    gd <-
      get_dashboard(
        api_token = api_token,
        workspace_id = workspace_id,
        since = since,
        until = until
      )
    if (isTRUE(project_name %in%  names(gd$tache))) {
      info <- gd$tache[[project_name]]
    } else{
      info <-
        structure(
          list(
            title = character(0),
            time = numeric(0),
            cur = numeric(0),
            sum = numeric(0),
            rate = numeric(0),
            local_start = character(0)
          ),
          row.names = integer(0),
          class = "data.frame"
        )
    }
    
    
    
    description <- get_current(api_token = api_token)$description
    
    if (!is.null(description)) {
      if (description %in% info$title) {
        info <-      info %>%
          mutate(time = case_when(
            title == description ~
              (time + get_current_duration(api_token = api_token)) %>% as.double()
            ,
            TRUE ~ time %>% as.double()
          ))
      } else {
        info <- info %>% full_join(by = c("title", "time"),tribble(
          ~ title,        ~ time,
          description,    get_current_duration(api_token = api_token) %>% as.double()
          
          
        ))
        
      }
      
      
      
    }
    
    
    
    if (humain & nrow(info)>0) {
      info <- info %>% to_humain()
    }
    info
  }

#' transforme time column into humain readable column
#'
#' @param df a data.frame
#'
#' @return the data frame, with `time` as a readable duration
#' @export
to_humain <- function(df){
  df %>% mutate(time = n_to_tps(time))
}