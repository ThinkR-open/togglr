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
#' @return the id of the project, or `NULL` when it does not exist (and is not created)
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
#' @param active `NULL` for the active projects only (the default of the API),
#'   `"false"` for the archived ones, `"both"` for all
#'
#' @importFrom httr GET authenticate content
#' @importFrom dplyr bind_rows filter rename select
#' @return a tibble with `id`, `project_name` and `active`; 0 rows when the
#'   workspace has no such project
#' @export
get_project_id_and_name <- function(api_token = get_toggl_api_token(),
                                    workspace_id = get_workspace_id(api_token),
                                    active = NULL) {
  if (is.null(api_token)) {
    stop("you have to set your api token using set_toggl_api_token('XXXXXXXX')")
  }
  projects <- get_all_projects(api_token = api_token, workspace_id = workspace_id,
                               active = active)
  if (!"id" %in% names(projects)) {
    return(dplyr::tibble(id = integer(0), project_name = character(0), active = logical(0)))
  }
  projects %>%
    select(id, name, "active") %>%
    rename(project_name = name)

}


#' Every project of a workspace
#'
#' `active`: `NULL` (the default of the API: active projects only), `"false"`
#' (archived projects only) or `"both"`.
#' @noRd
get_all_projects <- function(api_token = get_toggl_api_token(),
                             workspace_id = get_workspace_id(api_token),
                             max_pages = getOption("togglr.max_pages", 100),
                             active = NULL) {
  all_projects <- list()
  previous_ids <- NULL
  filter_active <- ""
  if (!is.null(active)) {
    filter_active <- paste0("&active=", active)
  }
  for (nb_page in seq_len(max_pages)) {
    project_tbl_ <- toggl_GET(
      paste0("https://api.track.toggl.com/api/v9/workspaces/", workspace_id,
             "/projects?page=", nb_page, "&per_page=200", filter_active),
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


#' Data of one project
#'
#' @param project_id the project id
#' @param api_token the toggl api token
#' @param workspace_id the workspace id
#' @return a list with the fields of the project (`id`, `name`, `client_id`,
#'   `active`, `billable`, `color`...), as returned by the API v9
#' @export
#' @examples
#' \dontrun{
#' get_project_data(get_project_id("my_project"))
#' }
get_project_data <- function(project_id,
                             api_token = get_toggl_api_token(),
                             workspace_id = get_workspace_id(api_token)) {
  toggl_GET(paste0("https://api.track.toggl.com/api/v9/workspaces/", workspace_id,
                   "/projects/", project_id), api_token = api_token)$content
}
