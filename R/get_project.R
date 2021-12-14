#' @title get_project_id
#' @description  retrieve project id
#'
#' @param project_name the project name
#' @param create boolean do you want to create the project if it doesnt exist ?
#' @param client client name
#' @param api_token the toggl api token
#' @param workspace_id workspace id
#' @param color id of the color selected for the project
#'
#' @importFrom httr GET authenticate content
#' @importFrom dplyr bind_rows filter
#' @export
get_project_id <- function(project_name = get_context_project(),
         api_token = get_toggl_api_token(),
         
         create = FALSE,
         client = NULL,
         workspace_id = get_workspace_id(api_token),color=NULL) {
  if (is.null(api_token)) {
    stop("you have to set your api token using set_toggl_api_token('XXXXXXXX')")
    
  }
  
  project_tbl <- content(GET(
    paste0(
      "https://api.track.toggl.com/api/v8/workspaces/",
      workspace_id,
      "/projects?active=both"
    ),
    # verbose(),
    authenticate(api_token, "api_token"),
    encode = "json"
  )) %>% bind_rows()
  
  if (ncol(project_tbl) == 0) {
    id <- NULL
  } else {
    project_tbl %>%
      filter(name == project_name) %>% .$id -> id
  }
  
  if (length(id) == 0) {
    warning(paste("the project ", project_name, " doesn't exist "))
    id <- NULL
    
  }
  if (is.null(id) & create) {
    if (is.null(client)){
      message("You have to specify a client to create a new project")
      return(NULL)
    }
    
    message(" we create the project")
    id <-  toggl_create_project(project_name = project_name,
                                api_token = api_token,
                                client = client,color = color)
  }
  
  id
  
}





#' @title get_project_id_and_name
#' @description  retrieve project id and name
#'
#' @param api_token the toggl api token
#' @param workspace_id workspace id
#'
#' @importFrom httr GET authenticate content
#' @importFrom dplyr bind_rows filter rename select
#' @export
get_project_id_and_name <- function(
                           api_token = get_toggl_api_token(),
                           workspace_id = get_workspace_id(api_token)
                          ) {
  if (is.null(api_token)) {
    stop("you have to set your api token using set_toggl_api_token('XXXXXXXX')")
    
  }
  
  # active: possible values true/false/both. By default true. If false, only archived projects are returned.
  
  content(GET(
    paste0(
      "https://api.track.toggl.com/api/v8/workspaces/",
      workspace_id,
      "/projects?active=both"
    ),
    # verbose(),
    authenticate(api_token, "api_token"),
    encode = "json"
  )) %>% 
    bind_rows()  %>% 
    select(id,name,'active') %>% 
    rename(project_name = name)

}