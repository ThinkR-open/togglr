# ---- M11 (#69) : simplify() sans fonction interne de jsonlite ------------------------

fixtures <- list(
  entries = list(
    list(id = 3600000001, pid = 1L, description = "a", tags = list("x", "y"), task = NULL, dur = 3600000L),
    list(id = 3600000002, pid = NULL, description = "b", tags = list(), task = "t", dur = 60000L)
  ),
  nested = list(list(title = list(project = "p", client = NULL), time = 12L, items = list(list(title = list(time_entry = "e"), time = 3L)))),
  empty = list(),
  scalars = list(total_count = 3L, per_page = 50L, data = list())
)

test_that("simplify() does not rely on an internal function of jsonlite (#69)", {
  # (`:::` n'est pas teste : covr instrumente les fonctions avec covr:::count)
  used <- all.names(body(simplify))
  expect_false("getFromNamespace" %in% used)
})

test_that("simplify() gives the same result as jsonlite's simplification (#69)", {
  ref <- get("simplify", envir = asNamespace("jsonlite"))
  for (x in fixtures) {
    expect_equal(simplify(x, simplifyDataFrame = TRUE), ref(x, simplifyDataFrame = TRUE))
  }
})

test_that("large ids keep their full precision (#69)", {
  out <- simplify(fixtures$entries, simplifyDataFrame = TRUE)
  expect_identical(out$id, c(3600000001, 3600000002))
})
