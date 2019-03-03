#' get detailled report
#' 
#' by user then projet
#'
#' @param api_token the toggl api token
#' @param workspace_id the workspace id
#' @param since begin date
#' @param until stop date
#'
#' @import httr
#' @importFrom glue glue
#' @importFrom lubridate years
#' @importFrom  dplyr select left_join as.tbl
#' @importFrom stats setNames
#' @importFrom purrr map
#' @encoding UTF-8
get_detailled_report_paged <- function(api_token = get_toggl_api_token(),
                               workspace_id = get_workspace_id(api_token),
                               since = Sys.Date() - lubridate::years(3),
                               until = Sys.Date()+lubridate::days(1),
                               page=1,
                               user_agent="togglr"#,
                               # users = get_workspace_users(api_token=api_token, workspace_id=workspace_id)
) {
  

    
    
 
  
  
  url <- glue("https://toggl.com/reports/api/v2/details?workspace_id={workspace_id}&since={since}&until={until}&user_agent={user_agent}&grouping=users&page={page}",
                    since=format(since, "%Y-%m-%dT00:00:00"),
                    until=format(until, "%Y-%m-%dT00:00:00")
  )
  wp <- content(GET(url,
                    # verbose(),
                    authenticate(api_token, "api_token"),
                    encode = "json"))

  togglr:::simplify(wp$data, simplifyDataFrame = TRUE) -> out
  
  
  
  out %>% as.tbl()
}

poss_get_detailled_report_paged <- purrr::possibly(get_detailled_report_paged,data.frame())
mem_poss_get_detailled_report_paged <- memoise::memoise(poss_get_detailled_report_paged)

#' @export
#' @rdname get_detailled_report_paged
get_detailled_report <- function(api_token = get_toggl_api_token(),
                               workspace_id = get_workspace_id(api_token),
                               since = Sys.Date() - lubridate::years(1),
                               until = Sys.Date(),
                               user_agent="togglr",
                               max_page=10
                               # users = get_workspace_users(api_token=api_token, workspace_id=workspace_id)
) {
  
  # message("until =",until)
  # message("since =",since)
  
  # si la plage plus longue que 1 ans on découpe
  
  
  if  ( difftime(until,since,units = "days") > 366){
    
    # ou alors demander des années civiles pour optimiser les temps
    
    out <- bind_rows(
    
    # get_detailled_report(until = until,since = until - years(1)),
    # get_detailled_report(until = until - years(1), since=since)
      
      
    get_detailled_report(until = until,since = dmy(glue("0101{year(until)}"))),
    get_detailled_report(until = dmy(glue("0101{year(until)}"))-days(1), since=since)
    
    
    )
  }else{
    message("until =",until)
    message("since =",since)
  
  res <- list()
  pb <- txtProgressBar()

i <- 1 
estimi <- 100
  repeat {
    # print(i)
    res[[i]] <- mem_poss_get_detailled_report_paged(api_token = api_token ,
                               workspace_id = workspace_id ,
                               since = since,until = until,
                               user_agent = user_agent,page = i)
    setTxtProgressBar(pb, i/estimi)
    
    if (nrow(res[[i]]) == 0){break}
    i <- i +1
  }
close(pb)
  
 out <-  bind_rows(res)
  }
  out
}
