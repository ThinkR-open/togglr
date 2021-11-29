#' @title toggl_update_entries
#' @description  update time entries
#' @param time_entry_ids id(s) of time entries to update
#' @param start time in POSIXt
#' @param stop time in POSIXt
#' @param duration in seconds
#' @param description the task you did
#' @param api_token the toggl api token
#' @param pid pid
#' @param tags tags
#' @importFrom lubridate now
#' @importFrom httr POST authenticate verbose
#' @importFrom jsonlite toJSON
#' @examples 
#' \dontrun{
#' options(toggl_api_token = "XXXXXXXX")# set your toggl api token here
#' time_entry <- toggl_create(duration=1200)
#' time_entry_id <- content(time_entry)$data$id
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
  api_token=get_toggl_api_token()){
  if (is.null(api_token)){
    
    stop("you have to set your api token using set_toggl_api_token('XXXXXXXX')")
    
  }
  
  if (missing(time_entry_ids)) {
    stop("you need to supply a time_entry_ids ('id' field in entry data)")
  }

  if ( !missing(tags) && !is.list(tags)){
    tags <- as.list(tags)
  }

  time_entry <- list(
    description = description,
    pid = pid,
    tags = tags,
    created_with = "togglr",
    duration=duration,
    start = correct_date(start),
    at = correct_date(now())
  )

  # Remove the null elements of the time_entry
  time_entry <- time_entry[!unlist(lapply(time_entry, is.null))]

  POST(
    paste0(
      "https://api.track.toggl.com/api/v8/time_entries/",
      # Separate multiple ids with commas
      paste(time_entry_ids, collapse = ",")
    ),
       verbose(),
       authenticate(api_token,"api_token"),
       encode="json",
       
       body=toJSON(list(time_entry = time_entry), auto_unbox = TRUE)
  )
}