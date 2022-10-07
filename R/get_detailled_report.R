#' get detailled report
#' 
#' get detailled report by user then projet
#'
#' @param api_token the toggl api token
#' @param workspace_id the workspace id
#' @param since begin date
#' @param until stop date
#' @param page page
#' @param user_agent user_agent
#' @param max_page max_page
#'
#' @import httr
#' @importFrom glue glue
#' @importFrom lubridate years
#' @importFrom dplyr select left_join as_tibble
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
  

    
    
 
  
  
  url <- glue("https://api.track.toggl.com/reports/api/v2/details?workspace_id={workspace_id}&since={since}&until={until}&user_agent={user_agent}&grouping=users&page={page}",
                    since=format(since, "%Y-%m-%dT00:00:00"),
                    until=format(until, "%Y-%m-%dT00:00:00")
  )
  wp <- content(GET(url,
                    # verbose(),
                    authenticate(api_token, "api_token"),
                    encode = "json"))

  togglr::simplify(wp$data, simplifyDataFrame = TRUE) -> out
  
  
  
  out %>% as_tibble()
}




#' @export
#' @param memoise_cache_dir cache folder for memoise function, can be edited with `options('togglr_memoise_dir')` or `rappdirs::user_cache_dir("togglr")` by default
#' @rdname get_detailled_report_paged
#' @importFrom lubridate dmy days year
#' @importFrom utils txtProgressBar setTxtProgressBar
get_detailled_report <- function(api_token = get_toggl_api_token(),
                               workspace_id = get_workspace_id(api_token),
                               since = Sys.Date() - lubridate::years(1),
                               until = Sys.Date(),
                               user_agent="togglr",
                               max_page=10,
                               memoise_cache_dir = getOption("togglr_memoise_dir",default = rappdirs::user_cache_dir("togglr"))
                               # users = get_workspace_users(api_token=api_token, workspace_id=workspace_id)
) {
  
  # message("until =",until)
  # message("since =",since)
  cd <- cachem::cache_disk(dir =  memoise_cache_dir)
poss_get_detailled_report_paged <- purrr::possibly(get_detailled_report_paged,data.frame())
mem_poss_get_detailled_report_paged <- memoise::memoise(poss_get_detailled_report_paged,
                                                        cache = cd)

  # si la plage plus longue que 1 ans on d?coupe
  
  
  if  ( difftime(until,since,units = "days") > 366){
    
    # ou alors demander des ann?es civiles pour optimiser les temps
    
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
  if (nrow(out) == 0){
    
    out <- structure(list(id = numeric(0), pid = integer(0), tid = logical(0), 
                          uid = integer(0), description = character(0), start = character(0), 
                          end = character(0), updated = character(0), dur = integer(0), 
                          user = character(0), use_stop = logical(0), client = character(0), 
                          project = character(0), project_color = character(0), project_hex_color = character(0), 
                          task = logical(0), billable = numeric(0), is_billable = logical(0), 
                          cur = character(0), tags = list()), row.names = integer(0), class = c("tbl_df", 
                                                                                                "tbl", "data.frame"))
  }
  
  out
}



#' clean memoise cache 
#'
#' @rdname get_detailled_report_paged
#'
#' @export
#'
#' @examples
#' \dontrun{
#' clean_memoise_cache()
#' }
clean_memoise_cache <- function(memoise_cache_dir = getOption("togglr_memoise_dir",default = rappdirs::user_cache_dir("togglr"))){
  
  unlink(memoise_cache_dir,recursive = TRUE,force = TRUE)
  
}