#' @title create_client
#' @description  create a client
#'
#' @param name client name
#' @param workspace_id workspace id
#' @param api_token the toggl api token
#'
#' @import glue
#' @importFrom httr POST authenticate
#' @importFrom magrittr %>%
#' @importFrom jsonlite toJSON
#' @examples 
#' \dontrun{
#' get_toggl_api_token()# set your api token here
#' create_client("new client")
#' }
#' @export
create_client <- function(name = "wihtout client",
                          api_token = get_toggl_api_token(),
                          workspace_id = get_workspace_id(api_token)
                          ){

# POST https://www.toggl.com/api/v8/clients
message(glue("we create the client : {name}"))
POST("https://www.toggl.com/api/v8/clients",
     verbose(),
     authenticate(api_token,"api_token"),
     encode="json",
     body=toJSON(
       list(client = list(
         name = name,
         wid = workspace_id
       )
       ),
       auto_unbox = TRUE)
)  
}


#' get_all_client_info
#'
#' @param api_token the toggl api token
#'
#' @return a data.frame
#' @export
#' @examples 
#' \dontrun{
#' get_all_client_info()
#' }
#' @export
get_all_client_info <- function(api_token=get_toggl_api_token()){
  GET("https://www.toggl.com/api/v8/clients",authenticate(api_token,"api_token")) %>% content() %>% bind_rows()
}



#' client_name_to_id
#'
#' return client id from client name
#' @param name client name
#' @param api_token the toggl api token
#' @import assertthat
#' @importFrom dplyr filter pull
#' @return the client id
#' @export
#'
client_name_to_id <- function(name, api_token = get_toggl_api_token()) {
  assertthat::assert_that(is.string(name))
  get_all_client_info(api_token = api_token) %>%
    filter(name == !!name) %>%
    pull("id")
}
#' client_id_to_name
#' 
#' return client name from client id
#'
#' @param id client id
#' @param api_token the toggl api token
#' @import assertthat
#' @importFrom dplyr filter pull
#' @return the client name
#' @export
#'
client_id_to_name <- function(id, api_token = get_toggl_api_token()) {
  get_all_client_info(api_token = api_token) %>%
    filter(id == !!id) %>%
    pull("name")
}


#' get_client_project
#'
#' @param id client id
#' @param api_token the toggl api token
#' @import assertthat
#' @importFrom dplyr filter pull
#' @return the client name
#' @export
#'
get_client_project <- function(id,api_token=get_toggl_api_token()){
  
  GET(glue("https://www.toggl.com/api/v8/clients/{id}/projects"),authenticate(api_token,"api_token")) %>% content() %>% bind_rows()
  

}
