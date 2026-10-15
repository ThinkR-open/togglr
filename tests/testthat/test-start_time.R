# ---- I8 (#52, PR #44) : heure de debut envoyee en UTC, quel que soit le fuseau ---

capture_start <- function(env = parent.frame()) {
  sent <- c()
  local_mocked_bindings(
    POST = function(url, ..., body) {
      sent <<- c(sent, jsonlite::fromJSON(body)$start)
      fake_response(200, list(id = 1L))
    },
    get_project_id = function(...) 1L,
    notification = function(...) NULL,
    .env = env
  )
  function() sent
}

instants <- list(
  ete_paris   = as.POSIXct("2026-07-01 10:00:00", tz = "Europe/Paris"),
  hiver_paris = as.POSIXct("2026-01-15 10:00:00", tz = "Europe/Paris"),
  utc         = as.POSIXct("2026-07-01 10:00:00", tz = "UTC"),
  new_york    = as.POSIXct("2026-07-01 10:00:00", tz = "America/New_York")
)
attendu <- c("2026-07-01T08:00:00Z", "2026-01-15T09:00:00Z",
             "2026-07-01T10:00:00Z", "2026-07-01T14:00:00Z")

test_that("toggl_start sends the start time in UTC, whatever its time zone (#52)", {
  sent <- capture_start()
  for (s in instants) toggl_start("x", start = s, api_token = "t", workspace_id = 1)
  expect_equal(sent(), attendu)
})

test_that("toggl_create sends the start time in UTC, whatever its time zone (#52)", {
  sent <- capture_start()
  for (s in instants) toggl_create("x", start = s, duration = 60, pid = 1L, api_token = "t", workspace_id = 1)
  expect_equal(sent(), attendu)
})

test_that("the result does not depend on the time zone of the session (#52)", {
  sent <- capture_start()
  withr::with_timezone("Asia/Tokyo",
                       toggl_start("x", start = instants$ete_paris, api_token = "t", workspace_id = 1))
  expect_equal(sent(), attendu[1])
})

# ---- #16 : tags de toggl_start() -----------------------------------------------------------

test_that("toggl_start sends its tags (#16)", {
  tags <- NULL
  local_mocked_bindings(
    POST = function(url, ..., body) { tags <<- jsonlite::fromJSON(body)$tags; fake_response(200, list(id = 1L)) },
    get_project_id = function(...) 1L, notification = function(...) NULL, toggl_sleep = function(s) NULL)
  local_fresh_state()
  toggl_start("x", tags = c("dev", "R"), api_token = "t", workspace_id = 1)
  expect_equal(tags, c("dev", "R"))
})
