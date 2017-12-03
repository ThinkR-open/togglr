
#' @title toggl_create_project
#' @param project_name project name
#' @param wid workspace_id
#' @param client client name
#' @param api_token the toggl api token#'
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
  wid=get_workspace_id(),
  client = NULL
  ){
  if (is.null(api_token)){
    stop("you have to set your api token using set_toggl_api_token('XXXXXXXX')")
    
  }
  
  if (!is.null(id<-suppressWarnings(get_project_id(project_name = project_name)))){
    warning("the project " ,project_name," already exist")
    
    
  }else{
  
    # gestion du client
    create_client(name = client,api_token = api_token,wid = wid)
    client_id <- client_name_to_id(name = client,api_token = api_token)
    
    
    
    
    
  POST("https://www.toggl.com/api/v8/projects",
       verbose(),
       authenticate(api_token,"api_token"),
       encode="json",
       body=toJSON(list(project = list(name = project_name , 
                                       cid = client_id)
       ),auto_unbox = TRUE)
  )%>%  content() %>% .$data %>% .$id -> id
  }
  
 # content %>% .$data %>% .$id -> id
  id
  
}


