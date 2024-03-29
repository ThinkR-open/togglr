#' @title toggl_start
#' @description  start a task

#'
#' @param description the task you are doing
#' @param start start time in POSIXt 
#' @param project_name nom du projet
#' @param api_token the toggl api token
#' @param client client name
#' @param workspace_id workspace id
#' @param tags tags
#'
#' @importFrom lubridate now
#' @importFrom httr POST authenticate content
#' @importFrom magrittr %>%
#' @importFrom jsonlite toJSON
#' @examples 
#' \dontrun{

#' get_toggl_api_token()# set your api token here
#' toggl_start()
#' }
#' @export
toggl_start <- function(description = get_context(),
                        client = "without client",
                        project_name = get_context_project(),
                        start = now(),
                        api_token = get_toggl_api_token(),
                        tags = "",
                        workspace_id = get_workspace_id(api_token)) {
  if (is.null(api_token)) {
    stop("you have to set your api token using set_toggl_api_token('XXXXXXXX')")
  }
  
 # je n'aime pas devoir faire cette bidouille
  current_time_with_offset <- as.POSIXlt(start) - 3600
  formatted_time <- format(current_time_with_offset, "%Y-%m-%dT%H:%M:%SZ")
  # correct_date me semble a utiliser
  
  
  POST(
    
    glue::glue("https://api.track.toggl.com/api/v9/workspaces/{workspace_id}/time_entries"),
    # verbose(),
    authenticate(api_token, "api_token")
    ,
    encode = "json"
    ,
    body = toJSON(list(
      duration=-1,
        start = formatted_time,
        created_with = "togglr",
        wid = workspace_id,
        description = description,
        tags = as.list(tags),
      # time_entry = list(
        pid = get_project_id(
          project_name = project_name,
          create = TRUE,
          client = client
        ),
        duronly = FALSE 
      # )
    ),
    auto_unbox = TRUE)
  )->ll 
  ll %>% content()  %>% .$id -> id
  

  if (!is.null(id)){
    notification(paste(description," START"),c("at :",
                                               as.character(start)
                                               
    ))
  }else{
    notification(
          title = paste(description," ERROR")
          ,msg = c("NOT STARTED"
        ))
  }
  
  invisible(id)

  
}



#' @title toggl_stop
#' @description  stop the active task
#' @param current list task id and start time
#' @param api_token the toggl api token
#' @param workspace_id workspace id
#' @importFrom httr PUT
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd_hms
#' @importFrom prettyunits pretty_dt
#' @examples 
#' \dontrun{
#' options(toggl_api_token = "XXXXXXXX")# set your api token here
#' toggl_start()
#' toggl_stop()
#' }
#' @export
toggl_stop <- function(current=get_current(),
                       api_token=get_toggl_api_token(),
                       workspace_id = get_workspace_id(api_token)){
  if (is.null(api_token)){

    stop("you have to set your api token using set_toggl_api_token('XXXXXXXX')")
    
  }
  if (is.null(current$id)){
    
    stop("i can't find any task to stop...")
    
  }
  PATCH(
   glue::glue("https://api.track.toggl.com/api/v9/workspaces/{workspace_id}/time_entries/{current$id}/stop"),
       # verbose(),
       authenticate(api_token,"api_token"),
       encode="json")
  notification(
    title = paste(current$description," STOP")
    ,msg = c("duration :",
            pretty_dt(now() - ymd_hms(current$start))
    )
  )

  
  
  
}





#' @title toggl_create
#' @description  create a time entry
#' @param start time in POSIXt
#' @param stop time in POSIXt
#' @param duration in seconds
#' @param description the task you did
#' @param api_token the toggl api token
#' @param pid pid
#' @param tags tags
#' @param workspace_id workspace id
#' @importFrom lubridate now
#' @importFrom httr POST authenticate verbose
#' @importFrom jsonlite toJSON
#' @examples 
#' \dontrun{
#' options(toggl_api_token = "XXXXXXXX")# set your toggl api token here
#' toggl_create(duration=1200)
#' 
#'toggl_create( description="description",
#'              start=now(),
#'              pid = get_project_id(project_name = "projectname",
#'                                  create=TRUE,client = "client"),
#'              duration=1000,
#'              api_token=get_toggl_api_token())
#' 
#' 
#' 
#' 
#' }
#' @export
toggl_create <- function(
  description=get_context(),
  start=now(),
  pid = get_project_id(),
  stop,
  duration,
  tags=NULL,
  api_token=get_toggl_api_token(),
  workspace_id = get_workspace_id(api_token)){
  if (is.null(api_token)){

    stop("you have to set your api token using set_toggl_api_token('XXXXXXXX')")
    
  }


  if (missing(duration) & missing(stop)){
    stop("You must give at least duration or stop time")
    }

  if (missing(duration)){
    duration <- round(as.numeric(difftime(stop,start,units = "secs")))
  }

  if ( !missing(tags) && !is.list(tags)){
    tags <- as.list(tags)
  }

  
  # je n'aime pas devoir faire cette bidouille
  current_time_with_offset <- as.POSIXlt(start) - 3600
  formatted_time <- format(current_time_with_offset, "%Y-%m-%dT%H:%M:%SZ")
  # correct_date me semble a utiliser
  
  
  
  POST(
    
    glue::glue("https://api.track.toggl.com/api/v9/workspaces/{workspace_id}/time_entries"),
    # verbose(),
    authenticate(api_token, "api_token")
    ,
    encode = "json"
    ,
    body = toJSON(list(
      duration=duration,
      start = formatted_time,
      created_with = "togglr",
      wid = workspace_id,
      description = description,
      tags = as.list(tags),
      # time_entry = list(
        pid = pid,
        duronly = FALSE 
      # )
    ),
    auto_unbox = TRUE)
  )->ll 
  ll %>% content()  %>% .$id -> id
  
  
  if (!is.null(id)){
    notification(paste(description," START"),c("at :",
                                               as.character(start)
                                               
    ))
  }else{
    notification(
      title = paste(description," ERROR")
      ,msg = c("NOT STARTED"
      ))
  }
  
  invisible(id)
  
}




