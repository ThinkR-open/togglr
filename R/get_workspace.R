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
 ppp <- toggl_GET("https://api.track.toggl.com/api/v9/workspaces",
                  api_token = api_token)$content
  
  # si plusieurs workspaces : le premier (voir #23)
  id <- if (length(ppp) > 0) ppp[[1]]$id else NULL
  if (length(id) == 0) {
    stop("cant find workspace id - is your api token ok ?", call. = FALSE)
  }
  id

}
