
#' @title correct_date
#' @description  bidouille pour avoir iso 8601, pas propre mais la tisuit, osef
#' @param time as POSIXt
#' @export
correct_date <- function(time){
  paste0(gsub(" ","T",as.character(time)),"+01:00")
}

#' @title get_toggl_api_token
#' @description  return the toggle api token
#' @export
get_toggl_api_token <- function(){
  getOption("toggl_api_token")
  }
