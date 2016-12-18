

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
    description <- "I'm using R"# TODO trouver un truc fun
  }
  description
}


#' @title get_current
#' @description  retrieve current projet id
#' @param api_token the toggl api token
#' @importFrom httr GET authenticate content
#' @export
get_current <- function(api_token=getOption("toggl_api_token")){
  if (is.null(api_token)){
    stop("you have to set your api token using options(toggl_api_token = 'XXXXXXXX')")
    
  }
  content(GET("https://www.toggl.com/api/v8/time_entries/current",
              # verbose(),
              authenticate(api_token,"api_token"),
              encode="json"))$data
  
}
