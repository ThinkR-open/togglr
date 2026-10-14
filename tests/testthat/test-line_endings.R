# ---- M14 (#72) : fins de ligne LF -----------------------------------------------------

test_that("the sources use LF line endings (#72)", {
  src <- testthat::test_path("..", "..")
  skip_if_not(dir.exists(file.path(src, "R")), "package sources not available")
  files <- list.files(file.path(src, c("R", "man", "tests/testthat", "vignettes")),
                      pattern = "\\.(R|Rd|Rmd)$", full.names = TRUE)
  has_cr <- vapply(files, function(f) any(readBin(f, "raw", file.size(f)) == as.raw(13)), logical(1))
  expect_equal(basename(files[has_cr]), character(0))
})
