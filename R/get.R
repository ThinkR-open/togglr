#' @title get_context
#' @description  retrieve Rstudio projet if possible
#' @importFrom rstudioapi getActiveProject
#' @export
get_context <- function(){
  projet <- NULL
  try(projet <- getActiveProject(),silent=TRUE)
  if (!is.null(projet)){
    description <- paste0("projet R ", basename(projet))
  }else{
    description <- "I'm using R"
  }
  description
}


#' @title get_current
#' @description  retrieve current projet id
#' @param api_token the toggl api token
#' @importFrom httr GET authenticate content
#' @export
get_current <- function(api_token=get_toggl_api_token()){
  if (is.null(api_token)){

    stop("you have to set your api token using set_toggl_api_token('XXXXXXXX')")
    
  }
  content  (
    
    GET("https://api.track.toggl.com/api/v9/me/time_entries/current",
              # verbose(),
              authenticate(api_token,"api_token"),
              encode="json")
          
          
          )
  
}




#' @title get_context_projet
#' @description  retrieve Rstudio projet if possible
#' @importFrom rstudioapi getActiveProject
#' @export
get_context_project <- function(){
  projet <- NULL
  try(projet <- getActiveProject(),silent=TRUE)
  if (!is.null(projet)){
    description <- basename(projet)
  }else{
    description <- "sans projet"
  }
  description
}

