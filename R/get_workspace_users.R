#' Title
#'
#' @param api_token the toggl api token
#' @param workspace_id the workspace id
#'
#' @export
#' @importFrom purrr keep
get_workspace_users <- function(api_token = get_toggl_api_token(),
                                workspace_id = get_workspace_id(api_token)){
  

  url <- glue::glue("https://api.track.toggl.com/api/v9/workspaces/{workspace_id}/users")

  
  wp <- toggl_GET(url, api_token = api_token)$content
  wp %>% 
    bind_rows()
}
