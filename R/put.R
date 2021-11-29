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
                        tags = NULL,
                        workspace_id = get_workspace_id(api_token)) {
  if (is.null(api_token)) {
    stop("you have to set your api token using set_toggl_api_token('XXXXXXXX')")
    
  }
  
 
 
  
  
  POST(
    "https://api.track.toggl.com/api/v8/time_entries/start",
    # verbose(),
    authenticate(api_token, "api_token"),
    encode = "json",
    body = toJSON(list(
      time_entry = list(
        description = description,
        tags = tags,
        created_with = "togglr",
        wid = workspace_id,
        pid = get_project_id(
          project_name = project_name,
          create = TRUE,
          client = client
        ),
        duronly = FALSE
      )
    ),
    auto_unbox = TRUE)
  ) %>% content() %>% .$data %>% .$id -> id
  
  
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
                       api_token=get_toggl_api_token()){
  if (is.null(api_token)){

    stop("you have to set your api token using set_toggl_api_token('XXXXXXXX')")
    
  }
  if (is.null(current$id)){
    
    stop("i can't find any task to stop...")
    
  }
  PUT(paste0("https://api.track.toggl.com/api/v8/time_entries/",current$id,"/stop"),

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
  api_token=get_toggl_api_token()){
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


  POST("https://api.track.toggl.com/api/v8/time_entries",
       verbose(),
       authenticate(api_token,"api_token"),
       encode="json",

       body=toJSON(list(time_entry = list(
         description = description,
         pid = pid,
         tags = tags,
                                   created_with = "togglr",
                                   duronly=FALSE,
                                   duration=duration,
                                   start = correct_date(start),
                                   at = correct_date(now())
       )
       ),auto_unbox = TRUE)
  )


}




