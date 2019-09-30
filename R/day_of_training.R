#' Add day of training
#'
#' @param name_tranning name  of compagny
#' @param day which day
#' @importFrom lubridate ymd_hms
#'
#' @export
#'
day_of_tranning <- function(name_tranning, day){
  start <- paste(day, " 09:00:00")
  end <- paste(day, " 16:00:00")
  toggl_create( 
    description = "Formation",
    pid = get_project_id(project_name = name_tranning),
  start= ymd_hms(start,tz = "Europe/Paris"),
  stop= ymd_hms(end,tz = "Europe/Paris"),
  tags="Formation")
}
