
#' @title toggl_create_project
#' @param project_name project name
#'
#' @param workspace_id workspace_id
#' @param client client name: an existing client is reused, an unknown one is
#'   created; `NULL` or `""` for no client
#' @param api_token the toggl api token
#' @param private whether project is accessible for only project users or for all workspace users (boolean, default false)
#' @param color color of the project, as a hex code (e.g. "#06aaf5")
#' @param active boolean set project as active, TRUE by default
#'
#' @description  create a project
#' @return the id of the project (created, or already existing)
#' @importFrom jsonlite toJSON
#' @examples 
#' \dontrun{
#' toggl_create_project()
#' }
#' @export
toggl_create_project <- function(
  project_name=get_context_project(),
  api_token=get_toggl_api_token(),
  workspace_id=get_workspace_id(api_token),
  client = NULL,
  private= FALSE, 
  color = NULL,active=TRUE
){
  if (is.null(api_token)){
    stop("you have to set your api token using set_toggl_api_token('XXXXXXXX')")
    
  }
  
  if (!is.null(id<-suppressWarnings(get_project_id(project_name = project_name, api_token = api_token, workspace_id = workspace_id)))){
    warning("the project " ,project_name," already exist")
    
  }else{
    
    # client : existant -> son id ; inconnu -> cree une seule fois ; NULL / "" -> aucun
    client_id <- NULL
    if (!is.null(client) && nzchar(client)) {
      client_id <- client_name_to_id(name = client, api_token = api_token, workspace_id = workspace_id)
      if (length(client_id) == 0) {
        client_id <- create_client(name = client, api_token = api_token,
                                   workspace_id = workspace_id)$id
      }
      client_id <- client_id[1]
    }

    # corps au format v9 (champs a plat)
    info <- list(name = project_name, active = active, is_private = private,
                 color = color, client_id = client_id)
    info <- info[!vapply(info, is.null, logical(1))]

    id <- toggl_request("POST",
                        glue::glue("https://api.track.toggl.com/api/v9/workspaces/{workspace_id}/projects"),
                        api_token = api_token,
                        body = toJSON(info, auto_unbox = TRUE))$content$id

  }
  
  id
  
}


