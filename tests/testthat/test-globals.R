# ---- M12 (#70) : globalVariables ne masque pas des fonctions --------------------------

test_that("no declared global variable is a function name (it would hide a missing import) (#70)", {
  declared <- utils::globalVariables(package = "togglr")
  ns <- asNamespace("togglr")
  # fonctions du package ou importees (pas base / stats, ou des colonnes comme
  # `time` ou `start` portent le nom d'une fonction)
  is_fun <- vapply(declared, function(v) {
    exists(v, envir = ns, mode = "function", inherits = FALSE) ||
      exists(v, envir = parent.env(ns), mode = "function", inherits = FALSE)
  }, logical(1))
  expect_equal(names(which(is_fun)), character(0))
})

test_that("no declared global variable is unused in the code (#70)", {
  declared <- setdiff(utils::globalVariables(package = "togglr"), ".")
  ns <- asNamespace("togglr")
  used <- unique(unlist(lapply(ls(ns, all.names = TRUE), function(f) {
    obj <- get(f, envir = ns)
    if (is.function(obj)) all.names(body(obj)) else character(0)
  })))
  expect_equal(setdiff(declared, used), character(0))
})
