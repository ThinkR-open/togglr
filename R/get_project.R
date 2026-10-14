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
  
  # TODO la V9 permet de ne pas faire comme ceci et de faire la requete directement.
  
  

  project_tbl <- get_all_projects(api_token = api_token, workspace_id = workspace_id)
    
    
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
                                workspace_id = workspace_id,
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
  
get_all_projects(api_token = api_token, workspace_id = workspace_id) %>% 
    select(id,name,'active') %>% 
    rename(project_name = name)

}


get_all_projects <- function(api_token = get_toggl_api_token(),
                             workspace_id = get_workspace_id(api_token),
                             max_pages = getOption("togglr.max_pages", 100)) {
  all_projects <- list()
  previous_ids <- NULL
  for (nb_page in seq_len(max_pages)) {
    project_tbl_ <- toggl_GET(
      paste0("https://api.track.toggl.com/api/v9/workspaces/", workspace_id,
             "/projects?page=", nb_page, "&per_page=200"),
      api_token = api_token
    )$content %>% bind_rows()

    ids <- if ("id" %in% names(project_tbl_)) project_tbl_$id else NULL
    if (length(ids) > 0 && identical(ids, previous_ids)) {
      stop("the toggl API returned the same page of projects twice (page ", nb_page,
           "): pagination looks ignored, stopping.", call. = FALSE)
    }
    previous_ids <- ids
    all_projects[[nb_page]] <- project_tbl_

    if (nrow(project_tbl_) < 200) {
      return(bind_rows(all_projects))
    }
  }
  stop("more than ", max_pages, " pages of projects (option togglr.max_pages): stopping.",
       call. = FALSE)
}
