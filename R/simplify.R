#' simplify copied from jsonlite
#' @param ... Other parameters
#' @importFrom utils getFromNamespace
#' @export
simplify <- function(...){
  getFromNamespace('simplify','jsonlite')(...)
}