#' Simplify a parsed JSON answer into vectors and data frames
#'
#' Same result as `jsonlite::fromJSON(simplifyVector = TRUE)` on a list already
#' parsed with `simplifyVector = FALSE` (what `httr::content()` returns), using
#' only the public API of jsonlite: the list is serialised back to JSON at full
#' precision, then read with simplification.
#'
#' @param x a list, as returned by `httr::content()`
#' @param ... passed to `jsonlite::fromJSON()` (e.g. `simplifyDataFrame`)
#' @return the simplified object
#' @importFrom jsonlite toJSON fromJSON
#' @export
simplify <- function(x, ...) {
  json <- toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
  fromJSON(json, ...)
}
