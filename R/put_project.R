
#' @title toggl_create_project
#' @param project_name project name
#'
#' @param workspace_id workspace_id
#' @param client client name
#' @param api_token the toggl api token#'
#' @param private whether project is accessible for only project users or for all workspace users (boolean, default false)
#' @param color id of the color selected for the project
#' @param active boolean set project as active, TRUE by default
#'
#' @description  create a project
#' @importFrom httr POST authenticate verbose
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
  
  if (!is.null(id<-suppressWarnings(get_project_id(project_name = project_name)))){
    warning("the project " ,project_name," already exist")
    
  }else{
    
    # gestion du client
    if (!is.null(client)) {
      create_client(name = client,
                    api_token = api_token,
                    workspace_id = workspace_id)
      try(client_id <-
            client_name_to_id(name = client, api_token = api_token))
    } else{
      client_id <- NULL
    }
    
    
    info <- list(active=active,
                 name = project_name ,
                 # client_name = "client",
                 is_private = FALSE, 
                 project =
                   list(color = color,
                        active = active
                   )
    )
    
    
    if (client != "" & !is.null(client)){
      info$client <- client
          }
    
    POST(
      
      glue::glue("https://api.track.toggl.com/api/v9/workspaces/{workspace_id}/projects"),
         verbose(),
         authenticate(api_token,"api_token"),
         encode="json",
         body=toJSON(
           info
           
           ,auto_unbox = TRUE)
    ) ->ll
    
  ll  %>%  content()%>% .$id -> id

  }
  
  id
  
}


