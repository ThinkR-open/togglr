#' show notification
#'
#' show notification using message
#' @param title title of the notificatin
#' @param msg content of the notification
#'
#' @return invisibly, `NULL`; called for its message
#' @export
#'
notification <- function(title,msg){
  
  # if (requireNamespace("notifier", quietly = TRUE)){
  #   notifier::notify(
  #     title = paste(title," START")
  #     ,msg = msg
  #   )}else{
      
      message(title," - ",msg)
      
      
  # }
  
  
  
  
}