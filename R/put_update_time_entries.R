#' @title toggl_update_entries
#' @description  update time entries
#' @param time_entry_ids id(s) of time entries to update: one PUT request is
#'   sent per entry
#' @param start time in POSIXt
#' @param stop time in POSIXt
#' @param duration in seconds
#' @param description the task you did
#' @param api_token the toggl api token
#' @param workspace_id workspace id
#' @param pid project id
#' @param tags tags
#' @importFrom lubridate now
#' @return the updated entry, as returned by the API, or a list of them when
#'   several ids are given
#' @importFrom jsonlite toJSON
#' @examples 
#' \dontrun{
#' options(toggl_api_token = "XXXXXXXX")# set your toggl api token here
#' time_entry_id <- toggl_create(duration=1200)
#' 
#' toggl_update_entries( time_entry_id,
#'                     description = "new description",
#'                     duration = 100,
#'                     tags = c("tag1", "tag2"),
#'                     api_token=get_toggl_api_token())
#' 
#' }
#' @export
toggl_update_entries <- function(
  time_entry_ids,
  description=NULL,
  start=NULL,
  pid = NULL,
  stop = NULL,
  duration = NULL,
  tags=NULL,
  api_token=get_toggl_api_token(),
  workspace_id = get_workspace_id(api_token)){
  if (is.null(api_token)){
    
    stop("you have to set your api token using set_toggl_api_token('XXXXXXXX')")
    
  }
  
  if (missing(time_entry_ids)) {
    stop("you need to supply a time_entry_ids ('id' field in entry data)")
  }
  
  ids <- unlist(time_entry_ids)

  if (!is.null(tags) && !is.list(tags)) {
    tags <- as.list(tags)
  }

  time_entry <- list(
    description = description,
    project_id = pid,
    tags = tags,
    created_with = "togglr",
    duration = duration,
    start = correct_date(start),
    stop = correct_date(stop)
  )
  # Remove the null elements of the time_entry
  time_entry <- time_entry[!vapply(time_entry, is.null, logical(1))]
  body <- toJSON(time_entry, auto_unbox = TRUE)

  # v9 : un PUT par entree (idempotent, donc relancable sans risque)
  res <- lapply(ids, function(id) {
    toggl_request("PUT",
                  glue::glue("https://api.track.toggl.com/api/v9/workspaces/{workspace_id}/time_entries/{id}"),
                  api_token = api_token, body = body)$content
  })
  if (length(ids) == 1) res[[1]] else res
}