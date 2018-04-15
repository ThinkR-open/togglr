#' open toggle.com website on profile page
#'
#' @export
#' @importFrom utils browseURL
#'
open_toggl_website_profile <- function(){
  message("opening https://toggl.com/app/profile")
  browseURL("https://toggl.com/app/profile")
  
}

#' open toggle.com website on timesheet page
#'
#' @export
#' @importFrom utils browseURL
#'
open_toggl_website_app <- function(){
  message("opening https://toggl.com/app")
  browseURL("https://toggl.com/app")
  
}