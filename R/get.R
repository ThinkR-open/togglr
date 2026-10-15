#' @title get_context
#' @description  retrieve Rstudio projet if possible
#' @importFrom rstudioapi getActiveProject
#' @return a text describing the RStudio project, or "I'm using R" outside a project
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
#' @return a list describing the running time entry (`id`, `description`, `start`, `project_id`...), or `NULL` when nothing is running
#' @export
get_current <- function(api_token=get_toggl_api_token()){
  if (is.null(api_token)){

    stop("you have to set your api token using set_toggl_api_token('XXXXXXXX')")
    
  }
  toggl_GET("https://api.track.toggl.com/api/v9/me/time_entries/current",
            api_token = api_token)$content
  
}




#' @title get_context_projet
#' @description  retrieve Rstudio projet if possible
#' @importFrom rstudioapi getActiveProject
#' @return the name of the RStudio project, or "sans projet" outside a project
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

