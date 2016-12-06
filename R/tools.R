
#' @title correct_date
#' @description  bidouille pour avoir iso 8601, pas propre mais la tisuit, osef
#' @param time as POSIXt
#' @export
correct_date <- function(time){
  paste0(gsub(" ","T",as.character(time)),"+01:00")
}
