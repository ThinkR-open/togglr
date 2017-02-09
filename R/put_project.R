
#' @title toggl_create_project
#' @param project_name 
#' @param api_token the toggl api token#'
#' @description  create a project
#' @importFrom httr POST authenticate verbose
#' @importFrom jsonlite toJSON
#' @examples 
#' \dontrun{
#'
#' }
#' @export
toggl_create_project <- function(
  project_name=get_context_project(),
  api_token=get_toggl_api_token()){
  if (is.null(api_token)){
    stop("you have to set your api token using set_toggl_api_token('XXXXXXXX')")
    
  }
  
  if (!is.null(id<-suppressWarnings(get_project_id(project_name = project_name)))){
    warning("the project " ,project_name," already exist")
    
    
  }else{
  
  POST("https://www.toggl.com/api/v8/projects",
       verbose(),
       authenticate(api_token,"api_token"),
       encode="json",
       body=toJSON(list(project = list(name = project_name )
       ),auto_unbox = TRUE)
  )%>%  content() %>% .$data %>% .$id -> id
  }
  
 # content %>% .$data %>% .$id -> id
  id
  
}


