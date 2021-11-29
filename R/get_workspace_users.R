#' Title
#'
#' @param api_token the toggl api token
#' @param workspace_id the workspace id
#'
#' @export
#' @importFrom purrr keep
get_workspace_users <- function(api_token = get_toggl_api_token(),
                                workspace_id = get_workspace_id(api_token)){
  

  url <- glue::glue("https://api.track.toggl.com/api/v8/workspaces/{workspace_id}/users")

  
  wp <- content(GET(url,
                    # verbose(),
                    authenticate(api_token, "api_token"),
                    encode = "json"))
  wp %>% 
    # map(~discard(.x,str_detect(names(.x),"blog"))) %>%
    map(~keep(.x,names(.x) %in% c("id","default_wid","email","fullname","image_url"))) %>%
    # map(~discard(.x,names(.x) %in% c("send_timer_notifications",
    #                                  "send_product_emails",
    #                                  "send_weekly_report",
    #                                  "openid_enabled",
    #                                  "openid_email"))) %>%
    bind_rows()
}
