#' get current duration
#'
#' @param api_token the Token API
#'
#' @return a difftime
#' @export
#' @importFrom lubridate now ymd_hms
#' @examples
#' \dontrun{
#' get_current_duration()
#' }
get_current_duration <-
  function(api_token = get_toggl_api_token()) {
    # now() -  (get_current(api_token = api_token)$at %>% ymd_hms())
    out <-
      difftime(now() ,  (get_current(api_token = api_token)$at %>% ymd_hms()), units = "secs") *
      1000
    class(out) <- "toggl_time"
    if (length(out) == 0) {
      out <- 0
    }
    out
  }


#' @noRd
#' @export
print.toggl_time <- function(x,...) {
  print(prettyunits::pretty_ms(as.numeric(x)))
}
