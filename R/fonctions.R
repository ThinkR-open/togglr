#' @title correct_date
#' @description  bidouille pour avoir iso 8601, pas propre mais la tisuit, osef
#' @param time as POSIXt
#' @export
correct_date <- function(time){
  paste0(gsub(" ","T",as.character(time)),"+01:00") # bidouille pour avoir iso 8601, on peut le lire mais pas l'ecrire avec lubridate...
}



#' @title toggl_create
#' @description  create a
#' @param start time in POSIXt
#' @param stop time in POSIXt
#' @param duration in seconds
#' @param api_token the toggl api token
#' @importFrom lubridate now
#' @importFrom httr POST authenticate verbose
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
       body=list(time_entry = list(description = description,
                                   created_with = "togglr",
                                   duronly=FALSE,
                                   duration=duration,
                                   start = correct_date(start),
                                   # stop = correct_date(stop),
                                   at = correct_date(now())
       )
       )
  )


}


#' @title toggl_start
#' @description  start a task
#' @param description the task you are doing
#' @param start start time in POSIXt 
#' @param api_token the toggl api token
#' @importFrom lubridate now
#' @importFrom httr POST authenticate
#' @examples 
#' options(toggl_api_token = "XXXXXXXX")# set your api token here
#' toggl_start()
#' @export
toggl_start <- function(
  description,
  start=now(),
  api_token=getOption("toggl_api_token")){

  if (missing(description)){
    description <- get_context()
  }

  POST("https://www.toggl.com/api/v8/time_entries/start",
       verbose(),
       authenticate(api_token,"api_token"),
       encode="json",
       body=list(time_entry = list(description = description,
                                   created_with = "togglr",
                                   duronly=FALSE)
       )
  )
}


#' @title get_context
#' @description  retrieve Rstudio projet if possible
#' @importFrom rstudioapi getActiveProject
#' @importFrom httr POST authenticate
#' @export
get_context <- function(){
  projet <- NULL
  try(projet <- rstudioapi::getActiveProject(),silent=TRUE)
  if (!is.null(projet)){
    description <- paste0("projet R ", basename(projet))
  }else{
    description <- "I'm using R"# TODO trouver un truc fun
  }
  description
  }
