#' Title
#'
#' @param title 
#' @param msg 
#'
#' @return
#' @export
#'
notification <- function(title,msg){
  
  if (requireNamespace("notifier", quietly = TRUE)){
    notifier::notify(
      title = paste(description," START")
      ,msg = c("at :",
               as.character(start)
               
      )
    )}else{
      
      message(title," - ",msg)
      
      
    }
  
  
  
  
}