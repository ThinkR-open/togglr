# ---- A8 (#84) : gestion du token dans le trousseau ----------------------------------------

local_keyring <- function(initial = "old_token", env = parent.frame()) {
  store <- new.env(); store$value <- initial
  local_mocked_bindings(
    key_get = function(...) if (is.null(store$value)) stop("no key") else store$value,
    key_set_with_value = function(service, password, ...) store$value <- password,
    key_delete = function(...) store$value <- NULL,
    .env = env
  )
  store
}

test_that("set_toggl_api_token stores the token and does not print it (#84)", {
  store <- local_keyring()
  expect_invisible(set_toggl_api_token("new_token"))
  expect_equal(store$value, "new_token")
  expect_silent(set_toggl_api_token("new_token"))
})

test_that("set_toggl_api_token with an invalid token keeps the stored one (#84)", {
  store <- local_keyring()
  expect_error(set_toggl_api_token(123))
  expect_equal(store$value, "old_token")
})

test_that("update_toggl_api_token cancelled by the user keeps the stored token (#84)", {
  store <- local_keyring()
  local_mocked_bindings(ask_toggl_api_token = function(...) NULL)
  update_toggl_api_token()
  expect_equal(store$value, "old_token")
})

test_that("update_toggl_api_token replaces the stored token (#84)", {
  store <- local_keyring()
  local_mocked_bindings(ask_toggl_api_token = function(...) "fresh")
  update_toggl_api_token()
  expect_equal(store$value, "fresh")
})

test_that("delete_toggl_api_token removes the key, silently when there is none (#84)", {
  store <- local_keyring()
  delete_toggl_api_token()
  expect_null(store$value)
  local_mocked_bindings(key_delete = function(...) stop("no key"))
  expect_silent(delete_toggl_api_token())
})

test_that("correct_date keeps the time zone offset; NULL stays NULL (#84)", {
  expect_null(correct_date(NULL))
  x <- as.POSIXct("2026-07-01 10:00:00", tz = "Europe/Paris")
  out <- correct_date(x)
  expect_match(out, "^2026-07-01T[0-9:]{8}[+-][0-9]{2}:[0-9]{2}$")
  expect_equal(lubridate::ymd_hms(out), lubridate::with_tz(x, "UTC")) # meme instant
})
