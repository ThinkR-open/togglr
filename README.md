# togglr

an R and Rstudio wrapper for toggl Api.
<https://www.toggl.com/>

```R
library(togglr)

# go to https://toggl.com/app/profile and copy your API token

set_toggl_api_token("PASTE_YOUR_TOEKN_HERE")

toggl_start()
open_toggl_website() #browseURL("https://www.toggl.com/app/timer")



```




## Installation


```R
# install.packages("devtools")
devtools::install_github("ThinkR-open/togglr")



```
