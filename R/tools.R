









#' @title get_toggl_api_token
#' @description  return the toggle api token
#' @param ask booleen do we have to ask if missing
#' @importFrom magrittr %>% 
#' @export
get_toggl_api_token <- function(ask=TRUE){
  # getOption("toggl_api_token")
  token <-NULL
  try(token<-agent::agent_get("toggl_api_token"),silent=TRUE)
  if ( is.null(token) & ask){
    agent::agent_del("toggl_api_token")
    token <- ask_toggl_api_token() 
    token %>% agent::agent_set("toggl_api_token",.)
    
  }
  token
}


#' @title set_toggl_api_token
#' @description  set the toggle api token
#' @param token toggl api token
#' @importFrom magrittr %>% 
#' @import assertthat
#' @export
set_toggl_api_token <- function(token){
  # getOption("toggl_api_token")

  
  if ( missing(token) ){
    token <- ask_toggl_api_token() 
  }
  if (is.null(token)){return(invisible(NULL))}
  
  agent::agent_del("toggl_api_token")
  assert_that(is.character(token))
    token %>% agent::agent_set("toggl_api_token",.)
  
  token
}

#' @title update_toggl_api_token
#' @description  update the toggle api token
#' @importFrom magrittr %>% 
#' @export
update_toggl_api_token <- function(){
  # getOption("toggl_api_token")
  agent::agent_del("toggl_api_token")
    ask_toggl_api_token() %>% agent::agent_set("toggl_api_token",.)
    
}

#' @title delete_toggl_api_token
#' @description  delete the toggle api token
#' @export
delete_toggl_api_token <- function(){
  # getOption("toggl_api_token")
  agent::agent_del("toggl_api_token")
  
}


#'
#' @title ask_toggl_api_token
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



#' @title correct_date
#' @description  tricks to obtain iso 8601
#'
#' @param time as POSIXt
correct_date <- function(time){
  paste0(gsub(" ","T",as.character(time)),"+01:00")
}
