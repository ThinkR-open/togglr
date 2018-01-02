#' Get all time entries between 2 dates
#'
#' @param start start time
#' @param end end time
#'
#' @return a lit
#' @export
#' @importFrom lubridate years
#' @importFrom  dplyr select one_of
#' @importFrom stats setNames
#' @importFrom purrr map
#' @encoding UTF-8
#' @examples
#' get_time_entries()
get_time_entries <- function(api_token = get_toggl_api_token(),
                             workspace_id = get_workspace_id(),
                             since = Sys.Date() - lubridate::years(1),
                             until = Sys.Date()){
  # GET https://www.toggl.com/api/v8/time_entries
  
  wp <- content(GET(url,
                    # verbose(),
                    authenticate(api_token, "api_token"),
                    encode = "json"))
  
  
  # jsonlite:::simplify(wp$data, simplifyDataFrame = TRUE) -> out
  simplify(wp$data, simplifyDataFrame = TRUE) -> out
  
  
}