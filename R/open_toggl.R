#' open toggle.com website
#'
#' @export
#' @importFrom utils browseURL
#'
open_toggl_website <- function(){
  message("opening https://toggl.com/app/")
  browseURL("https://toggl.com/app/")
  
}