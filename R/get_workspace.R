#' @title get_workspace_id
#' @description Return the workspace id
#' @param api_token the toggl api token
#'
#' @importFrom httr GET authenticate content
#' @importFrom dplyr bind_rows
#' @export
get_workspace_id <- function(
  api_token = get_toggl_api_token()){
  if (is.null(api_token)){
    stop("you have to set your api token using set_toggl_api_token('XXXXXXXX')")

  }
 ppp <- content(GET("https://api.track.toggl.com/api/v9/workspaces",
              # verbose(),
              authenticate(api_token,"api_token"),
              encode="json")) 
  
 ppp[[1]]$id -> id # si plusieurs workspace , il faudra adapter
  if (length(id) == 0){
    stop(paste("cant find workspace id - is your api token ok ?"))
    id <- NULL
  }
  id

}
