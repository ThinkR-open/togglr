#' get_toggl_api_token
#' @description  return the toggle api token
#' @param ask booleen do we have to ask if missing
#' @importFrom magrittr %>% 
#' @importFrom  keyring key_set_with_value
#' @import keyring
#' @export
get_toggl_api_token <- function(ask=TRUE){
  token <-NULL
  
  try(token<-key_get(service = "toggl_api_token"),silent=TRUE)
  
  
  if ( is.null(token) & ask){
    delete_toggl_api_token()
    token <- ask_toggl_api_token() 
    token %>% key_set_with_value(service = "toggl_api_token",password = .)
    
  }
  token
}


#' set_toggl_api_token
#' @description  set the toggle api token
#' @param token toggl api token
#' @importFrom magrittr %>% 
#' @import assertthat
#' @export
set_toggl_api_token <- function(token){

  
  if ( missing(token) ){
    token <- ask_toggl_api_token() 
  }
  if (is.null(token)){return(invisible(NULL))}
  
  delete_toggl_api_token()
  assert_that(is.character(token))
    token %>% key_set_with_value(service = "toggl_api_token",password = .)
  
  token
}

#' update_toggl_api_token
#' @description  update the toggle api token
#' @importFrom magrittr %>% 
#' @export
update_toggl_api_token <- function(){
  delete_toggl_api_token()
    ask_toggl_api_token() %>% key_set_with_value(service = "toggl_api_token",password = .)
}

#' @title delete_toggl_api_token
#' @description  delete the toggle api token
#' @export
delete_toggl_api_token <- function(){
  try(key_delete("toggl_api_token"),silent=TRUE)
}


#' ask_toggl_api_token
#' @param msg the message
#' @description  ask for the toggle api token
#' @import getPass
#' @export
ask_toggl_api_token <- function (msg="toggl api token") 
{
  passwd <- tryCatch({
    newpass <- getPass::getPass(msg)
  }, interrupt = NULL)
  if (!length(passwd) || !nchar(passwd)) {
    return(NULL)
  }
  else {
    return(as.character(passwd))
  }
}



#' correct_date
#' @description  tricks to obtain iso 8601
#' @return time in iso 8601
#'
#' @param time a POSIXt
correct_date <- function(time){
  if (! is.null(time)) {
    tm <- strftime(time, "%Y-%m-%dT%H:%M:%S")
    time_z <- strftime(time, "%z") # format +0200
    # Add : between hour and rest
    time_z_colon <- paste0(
      substring(time_z, 1, 3),
      ":",
      substring(time_z, 4, 5))

    paste0(tm, time_z_colon)
  } else {
    return(NULL)
  }
}
