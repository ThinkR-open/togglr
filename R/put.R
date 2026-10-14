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
#' @return invisibly, the id of the started time entry (`NA`, with a warning, when the API accepted it but its answer could not be read)
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
  
  formatted_time <- format_toggl_utc(start)
  
  
  body <- toJSON(list(
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
          client = client,
          api_token = api_token,
          workspace_id = workspace_id
        ),
        duronly = FALSE 
      # )
    ),
    auto_unbox = TRUE)
  id <- toggl_post_entry(body, description, api_token, workspace_id)
  

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
#' set_toggl_api_token() # once, stores your token in the keyring
#' toggl_start()
#' toggl_stop()
#' }
#' @return invisibly, the stopped time entry, as returned by the API
#' @export
toggl_stop <- function(current=get_current(api_token = api_token),
                       api_token=get_toggl_api_token(),
                       workspace_id = get_workspace_id(api_token)){
  if (is.null(api_token)){

    stop("you have to set your api token using set_toggl_api_token('XXXXXXXX')")
    
  }
  if (is.null(current$id)){
    
    stop("i can't find any task to stop...")
    
  }
  res <- toggl_request("PATCH",
                       glue::glue("https://api.track.toggl.com/api/v9/workspaces/{workspace_id}/time_entries/{current$id}/stop"),
                       api_token = api_token)
  notification(
    title = paste(current$description," STOP")
    ,msg = c("duration :",
            pretty_dt(now() - ymd_hms(current$start))
    )
  )
  invisible(res$content)

  
  
  
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
#' @importFrom httr POST authenticate
#' @importFrom jsonlite toJSON
#' @examples 
#' \dontrun{
#' set_toggl_api_token() # once, stores your token in the keyring
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
#' @return invisibly, the id of the created time entry (`NA`, with a warning, when the API accepted it but its answer could not be read)
#' @export
toggl_create <- function(
  description=get_context(),
  start=now(),
  pid = get_project_id(api_token = api_token, workspace_id = workspace_id),
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

  
  formatted_time <- format_toggl_utc(start)
  
  
  
  body <- toJSON(list(
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
  id <- toggl_post_entry(body, description, api_token, workspace_id)
  
  
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






#' Format an instant for the Toggl API: UTC, ISO 8601 with a `Z`
#'
#' The instant is converted, not relabelled: 10:00 in Paris in July is sent as
#' 08:00Z, whatever the time zone of the R session (#44, #52).
#' @importFrom lubridate with_tz
#' @noRd
format_toggl_utc <- function(x) {
  format(with_tz(as.POSIXct(x), "UTC"), "%Y-%m-%dT%H:%M:%SZ")
}

#' Create a time entry; on failure, notify then raise the error of the API
#' @noRd
toggl_post_entry <- function(body, description, api_token, workspace_id) {
  res <- tryCatch(
    toggl_request("POST",
                  glue::glue("https://api.track.toggl.com/api/v9/workspaces/{workspace_id}/time_entries"),
                  api_token = api_token, body = body),
    error = function(e) {
      notification(title = paste(description, " ERROR"), msg = c("NOT STARTED", conditionMessage(e)))
      stop(e)
    }
  )
  # l'API a accepte l'entree : un id illisible n'est pas un echec (le relancer
  # creerait un doublon)
  if (is.null(res$content$id)) {
    warning("the time entry \"", description, "\" was created, but its id could not be read.",
            call. = FALSE)
    return(NA_real_)
  }
  res$content$id
}
