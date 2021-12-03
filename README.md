
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/togglr)](https://cran.r-project.org/package=togglr)
[![](http://cranlogs.r-pkg.org/badges/togglr)](https://cran.r-project.org/package=togglr)
[![R-CMD-check](https://github.com/ThinkR-open/togglr/workflows/R-CMD-check/badge.svg)](https://github.com/ThinkR-open/togglr/actions)
<!-- badges: end -->

# togglr

An R and Rstudio wrapper for toggl Api. <https://www.toggl.com/>

Documentation site: <https://thinkr-open.github.io/togglr/>

## Installation of `togglr`

### From CRAN

``` r
install.packages("togglr")
```

### From Github

``` r
if (!requireNamespace("devtools")){install.packages("devtools")}
devtools::install_github("ThinkR-open/togglr")
```

## Set toggl Api token

Go on toogl.com website : `https://track.toggl.com/profile`

``` r
togglr::open_toggl_website_profile()
```

then select and copy your token api at the bottom of the page.

``` r
library(togglr)
set_toggl_api_token("your_token_api")
```

You just need to do this once.

## Start the tracking system

Without any parameters it will create a new project using your Rstudio
project name.

``` r
toggl_start()
```

By default the client name is “without client” you can choose (and
eventualy create a client) by using :

``` r
toggl_start(client = "my client")
```

But you can also choose the task and the project

``` r
toggl_start(client = "my client",
            description = "what I'm doing",
            project_name = "my project")
```

## Stop the tracking system

``` r
toggl_stop()
```

## Get total time passed on the current project

``` r
get_current_duration()# the current track
get_project_task_detail()# all the project (including the current track)
```

## Get all your dashboard

``` r
get_dashboard()
```

## Use Rstudio Addins

This package comes with 2 Rstudio addins ‘start toggl’ and ‘stop toggl’,
feel free to use keybindings for convenience.

## Some other uselfull functions are in this package

``` r
ls(package:togglr)
```
