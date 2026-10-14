#' get_toggl_api_token
#' @description  return the toggle api token
#' @param ask booleen do we have to ask if missing
#' @importFrom magrittr %>% 
#' @importFrom  keyring key_set_with_value
#' @import keyring
#' @export
get_toggl_api_token <- function(ask=TRUE){
  # never delete the stored key here: a locked keyring (cron) also makes
  # key_get() fail, and the token would be lost
  token <- tryCatch(key_get(service = "toggl_api_token"), error = function(e) NULL)

  if (is.null(token) && ask) {
    if (!is_interactive()) {
      stop("no toggl API token: none could be read from the keyring (service ",
           "\"toggl_api_token\"). Pass `api_token` explicitly, or set it with ",
           "set_toggl_api_token().", call. = FALSE)
    }
    token <- ask_toggl_api_token()
    if (!is.null(token)) key_set_with_value(service = "toggl_api_token", password = token)
  }
  token
}

# wrapper, so that tests can simulate a non interactive session
#' @noRd
is_interactive <- function() interactive()


#' set_toggl_api_token
#' @description  set the toggle api token
#' @param token toggl api token
#' @importFrom magrittr %>% 
#' @import assertthat
#' @export
set_toggl_api_token <- function(token){

  
  if ( missing(token) ){
    token <- ask_toggl_api_token() 
  }
  if (is.null(token)){return(invisible(NULL))}
  
  delete_toggl_api_token()
  assert_that(is.character(token))
    token %>% key_set_with_value(service = "toggl_api_token",password = .)
  
  token
}

#' update_toggl_api_token
#' @description  update the toggle api token
#' @importFrom magrittr %>% 
#' @export
update_toggl_api_token <- function(){
  delete_toggl_api_token()
    ask_toggl_api_token() %>% key_set_with_value(service = "toggl_api_token",password = .)
}

#' @title delete_toggl_api_token
#' @description  delete the toggle api token
#' @export
delete_toggl_api_token <- function(){
  try(key_delete("toggl_api_token"),silent=TRUE)
}


#' ask_toggl_api_token
#' @param msg the message
#' @description  ask for the toggle api token
#' @import getPass
#' @export
ask_toggl_api_token <- function (msg="toggl api token") 
{
  passwd <- tryCatch({
    newpass <- getPass::getPass(msg)
  }, interrupt = NULL)
  if (!length(passwd) || !nchar(passwd)) {
    return(NULL)
  }
  else {
    return(as.character(passwd))
  }
}



#' correct_date
#' @description  tricks to obtain iso 8601
#' @return time in iso 8601
#'
#' @param time a POSIXt
correct_date <- function(time){
  if (! is.null(time)) {
    tm <- strftime(time, "%Y-%m-%dT%H:%M:%S")
    time_z <- strftime(time, "%z") # format +0200
    # Add : between hour and rest
    time_z_colon <- paste0(
      substring(time_z, 1, 3),
      ":",
      substring(time_z, 4, 5))

    paste0(tm, time_z_colon)
  } else {
    return(NULL)
  }
}

#' Read a date argument: a `Date`, a date-time (taken as its day in its own time
#' zone) or a text "AAAA-MM-JJ"; anything else fails with a clear message
#' @noRd
as_toggl_date <- function(x, arg = deparse(substitute(x))) {
  out <- if (inherits(x, "Date")) {
    x
  } else if (inherits(x, "POSIXt")) {
    as.Date(format(x, "%Y-%m-%d"))
  } else if (is.character(x) && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", x[1])) {
    tryCatch(as.Date(x, format = "%Y-%m-%d"), error = function(e) as.Date(NA))
  } else {
    as.Date(NA)
  }
  if (length(out) != 1 || is.na(out)) {
    stop("`", arg, "` must be a Date, a date-time or a text \"AAAA-MM-JJ\", not ",
         paste(format(x), collapse = ", "), ".", call. = FALSE)
  }
  out
}

#' Read and check a period: `since` <= `until`
#' @noRd
as_toggl_period <- function(since, until) {
  since <- as_toggl_date(since, "since")
  until <- as_toggl_date(until, "until")
  if (since > until) {
    stop("`since` (", format(since), ") is after `until` (", format(until), ").", call. = FALSE)
  }
  list(since = since, until = until)
}

# wrapper, so that tests can choose the day
#' @noRd
toggl_today <- function() Sys.Date()
