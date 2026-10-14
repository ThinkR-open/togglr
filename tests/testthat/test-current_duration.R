# ---- I9 (#53) : duree de l'entree en cours depuis start, pas depuis at -------

test_that("the current duration runs from the start, not from the last edit (#53)", {
  local_mocked_bindings(
    get_current = function(...) list(id = 9L, start = "2026-10-14T08:00:00+00:00",
                                     at = "2026-10-14T09:30:00+00:00"),
    now = function(...) as.POSIXct("2026-10-14 10:00:00", tz = "UTC")
  )
  expect_equal(as.numeric(get_current_duration(api_token = "t")), 2 * 3600 * 1000)
})

test_that("without a running entry the current duration is 0 (#53)", {
  local_mocked_bindings(get_current = function(...) NULL)
  expect_equal(as.numeric(get_current_duration(api_token = "t")), 0)
})
