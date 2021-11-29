#' open toggle.com website on profile page
#'
#' @export
#' @importFrom utils browseURL
#'
open_toggl_website_profile <- function(){
  message("opening https://track.toggl.com/profile")
  browseURL("https://track.toggl.com/profile")
  
}

#' open toggle.com website on timesheet page
#'
#' @export
#' @importFrom utils browseURL
#'
open_toggl_website_app <- function(){
  message("opening https://track.toggl.com/timer")
  browseURL("https://track.toggl.com/timer")
  
}