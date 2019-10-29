library( togglr)
library( tidyverse)
togglr::get_current()
togglr::get_current_duration()
togglr::toggl_create_project()
togglr::get_all_client_info()
# pkgdown::build_site()


curl -v -u 1971800d4d82861d8f2c1651fea4d212:api_token -X GET "https://toggl.com/reports/api/v2/weekly?workspace_id=123&since=2013-05-19&until=2013-05-20&user_agent=api_test"


POST(
  "https://www.toggl.com/api/v8/projects",
  verbose(),
  authenticate(api_token, "api_token"),
  encode = "json",
  body = toJSON(list(
    project = list(name = project_name,
                   cid = client_id)
  ), auto_unbox = TRUE)
) %>% content()
library(togglr)
library(httr)
library(tidyverse)
library(jsonlite)
api_token = get_toggl_api_token()
workspace_id = get_workspace_id(api_token)

GET(
  "https://www.toggl.com/reports/api/v2/weekly",
  verbose(),
  authenticate(api_token, "api_token"),
  body = toJSON(list(
    user_agent = 'togglr'
  )),
  encode = "json") %>% content()
