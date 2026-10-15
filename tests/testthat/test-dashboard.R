# ---- M13 (#71) : user agent ------------------------------------------------------------
# (get_dashboard en Reports v3 : test-summary_v3.R)

test_that("requests carry a User-Agent header (#71)", {
  seen <- NULL
  local_mocked_bindings(GET = function(...) { seen <<- list(...); fake_response(200) })
  toggl_http_get("u", "t")
  ua <- unlist(lapply(Filter(function(a) inherits(a, "request"), seen), function(r) r$options$useragent))
  expect_match(ua, "^togglr/")
})
