#' @importFrom utils getFromNamespace
#' @noRd
simplify <- function(...){
  getFromNamespace('simplify','jsonlite')(...)
}