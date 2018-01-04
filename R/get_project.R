

#' @title get_project_id
#' @description  retrieve project id
#'
#' @param project_name the project name
#' @param create boolean do you want to create the project if it doesnt exist ?
#' @param client client name
#' @param api_token the toggl api token
#' @param workspace_id workspace id
#'
#' @importFrom httr GET authenticate content
#' @importFrom dplyr bind_rows filter
#' @export
get_project_id <- function(project_name = get_context_project(),
                           api_token = get_toggl_api_token(),
                           
                           create = FALSE,
                           client = NULL,
                           workspace_id = get_workspace_id(api_token)) {
  if (is.null(api_token)) {
    stop("you have to set your api token using set_toggl_api_token('XXXXXXXX')")
    
  }
  content(GET(
    paste0(
      "https://www.toggl.com/api/v8/workspaces/",
      workspace_id,
      "/projects"
    ),
    # verbose(),
    authenticate(api_token, "api_token"),
    encode = "json"
  )) %>% bind_rows() %>%
    filter(name == project_name) %>% .$id -> id
  
  if (length(id) == 0) {
    warning(paste("the project ", project_name, " don't exist"))
    id <- NULL
    
  }
  if (is.null(id) & create) {
    message("we create the project")
    id <-  toggl_create_project(project_name = project_name,
                           api_token = api_token,
                           client = client)
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
  content(GET(
    paste0(
      "https://www.toggl.com/api/v8/workspaces/",
      workspace_id,
      "/projects"
    ),
    # verbose(),
    authenticate(api_token, "api_token"),
    encode = "json"
  )) %>% 
    bind_rows()  %>% 
    select(id,name) %>% 
    rename(project_name = name)

}