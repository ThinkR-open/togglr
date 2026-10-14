# Session state of the package (next free slot for a reports call, known quota
# exhaustion). Kept in memory by default; shared between processes through a
# locked file when `options(togglr.state_file = )` is set.
.togglr_env <- new.env(parent = emptyenv())

#' Read and update the package state atomically
#'
#' `f` receives the current state (a list) and returns
#' `list(state = <new state>, value = <returned value>)`. With
#' `options(togglr.state_file = <path>)`, the state lives in that file and the
#' update runs under a file lock (`<path>.lock`): every R process using the same
#' file (callr / future / targets workers, cron jobs) shares the pacing of the
#' requests and the knowledge of an exhausted quota. Without the option, the
#' state is private to the R session.
#' @noRd
toggl_state_update <- function(f) {
  path <- getOption("togglr.state_file")
  if (is.null(path)) {
    res <- f(if (is.null(.togglr_env$state)) list() else .togglr_env$state)
    .togglr_env$state <- res$state
    return(res$value)
  }
  lock <- filelock::lock(paste0(path, ".lock"), timeout = 60000)
  if (is.null(lock)) stop("could not lock the togglr state file ", path, call. = FALSE)
  on.exit(filelock::unlock(lock), add = TRUE)
  state <- if (file.exists(path)) tryCatch(readRDS(path), error = function(e) list()) else list()
  if (!is.list(state)) state <- list()
  res <- f(state)
  tmp <- tempfile(tmpdir = dirname(path))
  saveRDS(res$state, tmp)
  file.rename(tmp, path)
  res$value
}

#' @noRd
toggl_state_get <- function(name) {
  toggl_state_update(function(st) list(state = st, value = st[[name]]))
}

#' @noRd
toggl_state_set <- function(name, value) {
  toggl_state_update(function(st) {
    st[name] <- list(value)
    list(state = st, value = invisible(value))
  })
}
