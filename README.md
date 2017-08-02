# thinkR

an R and Rstudio wrapper for toggl Api.
<https://www.toggl.com/>

```R
library(togglr)


# without agent :
options(toggl_api_token = "XXXXXXXX")
toggl_start()
browseURL("https://www.toggl.com/app/timer")

#with agent ( see <https://github.com/ropensci/agent>)
toggl_start()



```




## Installation



```R
# install.packages("devtools")
devtools::install_github("ThinkRstat/togglr")#without agent

devtools::install_github("ThinkRstat/togglr", ref="agent")# with agent

# if you can't install the notifier package

devtools::install_github("ThinkRstat/togglr", ref="without_notifier_with_agent")



```
