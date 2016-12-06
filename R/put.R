#' @title toggl_start
#' @description  start a task
#' @param description the task you are doing
#' @param start start time in POSIXt 
#' @param api_token the toggl api token
#' @importFrom lubridate now
#' @importFrom httr POST authenticate content
#' @importFrom magrittr %>%
#' @importFrom jsonlite toJSON
#' @examples 
#' options(toggl_api_token = "XXXXXXXX")# set your api token here
#' toggl_start()
#' @export
toggl_start <- function(
  description=get_context(),
  start=now(),
  api_token=getOption("toggl_api_token")){
  
  
  POST("https://www.toggl.com/api/v8/time_entries/start",
       verbose(),
       authenticate(api_token,"api_token"),
       encode="json",
       body=toJSON(
         list(time_entry = list(description = description,
                                created_with = "togglr",
                                duronly=FALSE)),
         auto_unbox = TRUE)
  ) %>% content() %>% .$data %>% .$id %>% invisible()
  
}



#' @title toggl_stop
#' @description  stop the active task
#' @param id task id
#' @param api_token the toggl api token
#' @importFrom httr PUT
#' @importFrom magrittr %>%
#' @examples 
#' options(toggl_api_token = "XXXXXXXX")# set your api token here
#' toggl_start()
#' toggl_stop()
#' @export
toggl_stop <- function(id=get_current(),
                       api_token=getOption("toggl_api_token")){
  
  PUT(paste0("https://www.toggl.com/api/v8/time_entries/",id,"/stop"),
       verbose(),
       authenticate(api_token,"api_token"),
       encode="json")
  
}





#' @title toggl_create
#' @description  create a
#' @param start time in POSIXt
#' @param stop time in POSIXt
#' @param duration in seconds
#' @param description the task you did
#' @param api_token the toggl api token
#' @importFrom lubridate now
#' @importFrom httr POST authenticate verbose
#' @importFrom jsonlite toJSON
#' @examples 
#' options(toggl_api_token = "XXXXXXXX")# set your toggl api token here
#' toggl_create(duration=1200)
#' @export
toggl_create <- function(
  description=get_context(),
  start=now(),
  stop,
  duration,
  api_token=getOption("toggl_api_token")){


  if (missing(duration) & missing(stop)){
    stop("You must give at least duration or stop time")
    }

  if (missing(duration)){
    duration <- round(as.numeric(difftime(stop,start,units = "secs")))
  }


  POST("https://www.toggl.com/api/v8/time_entries",
       verbose(),
       authenticate(api_token,"api_token"),
       encode="json",
       body=toJSON(list(time_entry = list(description = description,
                                   created_with = "togglr",
                                   duronly=FALSE,
                                   duration=duration,
                                   start = correct_date(start),
                                   at = correct_date(now())
       )
       ),auto_unbox = TRUE)
  )


}



