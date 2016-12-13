#' @importFrom utils packageDescription
.onAttach <- function(libname, pkgname) {
    # Runs when attached to search() path such as by library() or require()
    if (interactive()) {


        pdesc <- packageDescription(pkgname)
        packageStartupMessage('')
        packageStartupMessage(pdesc$Package, " ", pdesc$Version, " by Vincent Guyader")
        packageStartupMessage("->  For help type help('togglr')")
        packageStartupMessage('')
        
        
        
        if (!requireNamespace("notifier", quietly = TRUE)){
          try(source("https://install-github.me/gaborcsardi/notifier"),silent=TRUE) # crade mais bon...
          
        }
        if (!requireNamespace("agent", quietly = TRUE)){
          try(source("https://install-github.me/ropensci/agent"),silent=TRUE) # crade mais bon...
          
        }
            if ( is.null(togglr::get_toggl_api_token())){
        packageStartupMessage("  => you have to set your api token using set_toggl_api_token()")
        }
        
    }
}

# enleve les faux positifs du check
globalVariables(c(".","notify","notifier")) # faudra mettre les autres pour que le check ne s'enflamme pas trop a cause des NSE


