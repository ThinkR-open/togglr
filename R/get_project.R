

#' @title get_project_id
#' @description  retrieve projet id
#'
#' @param project_name the project name
#' @param create boolean do you want to create the project if it doesnt exist ?
#' @param client client name
#' @param api_token the toggl api token
#'
#' @importFrom httr GET authenticate content
#' @importFrom dplyr bind_rows filter
#' @export
get_project_id <- function(project_name = get_context_project(),
                           api_token = get_toggl_api_token(),
                           create = FALSE,
                           client = NULL) {
  if (is.null(api_token)) {
    stop("you have to set your api token using set_toggl_api_token('XXXXXXXX')")
    
  }
  content(GET(
    paste0(
      "https://www.toggl.com/api/v8/workspaces/",
      get_workspace_id(),
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
