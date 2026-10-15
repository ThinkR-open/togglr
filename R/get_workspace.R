#' @title get_workspace_id
#' @description Return the id of the default workspace of the account (as set
#'   in Toggl), or of its first workspace when no default is set.
#' @param api_token the toggl api token
#'
#' @return the workspace id (an integer)
#' @seealso [get_workspaces()] to list every workspace
#' @export
get_workspace_id <- function(
  api_token = get_toggl_api_token()){
  if (is.null(api_token)){
    stop("you have to set your api token using set_toggl_api_token('XXXXXXXX')")
  }
  me <- toggl_GET("https://api.track.toggl.com/api/v9/me", api_token = api_token)$content
  if (!is.null(me$default_workspace_id)) {
    return(me$default_workspace_id)
  }
  ppp <- toggl_GET("https://api.track.toggl.com/api/v9/workspaces",
                   api_token = api_token)$content
  id <- if (length(ppp) > 0) ppp[[1]]$id else NULL
  if (length(id) == 0) {
    stop("cant find workspace id - is your api token ok ?", call. = FALSE)
  }
  id
}

#' List the workspaces of the account
#'
#' @param api_token the toggl api token
#' @return a tibble with `id`, `name` and `default` (`TRUE` for the default
#'   workspace of the account)
#' @export
get_workspaces <- function(api_token = get_toggl_api_token()) {
  me <- toggl_GET("https://api.track.toggl.com/api/v9/me", api_token = api_token)$content
  ws <- toggl_GET("https://api.track.toggl.com/api/v9/workspaces", api_token = api_token)$content
  ids <- vapply(ws, function(w) as.integer(w$id), integer(1))
  dplyr::tibble(
    id = ids,
    name = vapply(ws, function(w) null_to_na(w$name, NA_character_), character(1)),
    default = ids %in% null_to_na(me$default_workspace_id, NA_integer_)
  )
}
