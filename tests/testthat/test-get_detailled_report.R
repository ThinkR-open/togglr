# Comportements du rapport detaille independants de la version de l'API
# (Reports v3 depuis #76 ; la pagination et la completude : test-detailled_v3.R)

local_v3 <- function(pages, totals, env = parent.frame()) {
  srv <- serve_v3(pages, totals)
  local_mocked_bindings(POST = srv$send, toggl_sleep = function(s) NULL, .env = env)
  local_fresh_state(env)
  srv
}

# ---- I2 (#46) : types de colonnes stables -----------------------------------

test_that("an empty period has the same column types as a filled one (#46)", {
  e <- empty_detailled_report()
  expect_type(e$task, "character")
  expect_type(e$tid, "integer")
  expect_type(e$id, "double")
  expect_type(e$dur, "double")
})

test_that("a period over a year with an empty year can be combined (#46)", {
  # 2016 est lue en premier, avec des taches ; 2015 est vide
  with_task <- structure(lapply(1:3, function(i) { r <- v3_row(i, task_id = 99L); r$task_name <- "a task"; r }),
                         more = FALSE)
  local_v3(list(with_task, structure(list(), more = FALSE)), totals = c(3 * 3600, 3 * 3600, 0, 0))
  out <- suppressMessages(get_detailled_report(api_token = "t", workspace_id = 1,
                                               since = as.Date("2015-06-01"),
                                               until = as.Date("2016-12-31")))
  expect_equal(nrow(out), 3)
  expect_equal(out$task, rep("a task", 3))
  expect_equal(out$tid, rep(99L, 3))
})

test_that("pages with and without tasks are combined with stable types (#46)", {
  p2 <- lapply(3:4, function(i) { r <- v3_row(i, task_id = 5L); r$task_name <- "t"; r })
  local_v3(list(v3_page(1:2, more = TRUE), structure(p2, more = FALSE)), totals = 4 * 3600)
  out <- suppressMessages(report())
  expect_type(out$task, "character")
  expect_type(out$tid, "integer")
  expect_equal(sum(!is.na(out$task)), 2)
})

# ---- I14 (#58) : decoupage au-dela d'un an ----------------------------------------


test_that("367 days (bounds included) are split at the new year (#58)", {
  asked <- local_periods()
  suppressMessages(get_detailled_report(api_token = "t", workspace_id = 1,
                                        since = as.Date("2016-01-01"), until = as.Date("2017-01-01")))
  expect_equal(asked(), c("2016-01-01 2016-12-31", "2017-01-01 2017-01-01"))
})

test_that("a full calendar year, even a leap one, is fetched in one go (#58)", {
  asked <- local_periods()
  suppressMessages(get_detailled_report(api_token = "t", workspace_id = 1,
                                        since = as.Date("2016-01-01"), until = as.Date("2016-12-31")))
  expect_equal(asked(), "2016-01-01 2016-12-31")
})

test_that("less than a year across a new year is fetched in one go (#58)", {
  asked <- local_periods()
  suppressMessages(get_detailled_report(api_token = "t", workspace_id = 1,
                                        since = as.Date("2016-03-01"), until = as.Date("2017-02-28")))
  expect_equal(asked(), "2016-03-01 2017-02-28")
})

test_that("several years give one chunk per calendar year, none longer than a year (#58)", {
  asked <- local_periods()
  out <- suppressMessages(get_detailled_report(api_token = "t", workspace_id = 1,
                                               since = as.Date("2015-06-01"), until = as.Date("2017-12-31")))
  expect_equal(asked(), c("2015-06-01 2015-12-31", "2016-01-01 2016-12-31", "2017-01-01 2017-12-31"))
  expect_equal(nrow(out), 3)
})

# ---- M2 (#60) : max_page deprecie -------------------------------------------------

test_that("max_page warns that it is ignored, and is ignored (#60)", {
  local_v3(list(v3_page(1:2, more = TRUE), v3_page(3)), totals = 3 * 3600)
  expect_warning(out <- suppressMessages(report(max_page = 1)), "max_page.*deprecated")
  expect_equal(nrow(out), 3)
})

test_that("no warning without max_page (#60)", {
  local_v3(list(v3_page(1:3)), totals = 3 * 3600)
  expect_no_warning(suppressMessages(report()))
})

# ---- M7 (#65) : nombre de lectures parametrable ------------------------------------

test_that("by default a period is read up to 3 times when it keeps moving (#65)", {
  srv <- local_v3(list(v3_page(1:2), v3_page(1:3), v3_page(1:4)),
                  totals = c(2, 3, 3, 4, 4, 4) * 3600)
  expect_equal(nrow(suppressMessages(report())), 4)
  expect_equal(srv$n_search(), 3)
})

test_that("togglr.report_attempts sets the number of reads (#65)", {
  withr::local_options(togglr.report_attempts = 1)
  srv <- local_v3(list(v3_page(1:2)), totals = c(2, 3) * 3600)
  expect_error(suppressMessages(report()), "incomplete")
  expect_equal(srv$n_search(), 1)
})

# ---- M9 (#67) : cache dedie, noms sans memoise --------------------------------------

test_that("cache = TRUE stores only complete periods and reuses them (#67)", {
  dir <- withr::local_tempdir()
  withr::local_options(togglr.report_attempts = 1)
  local_v3(list(v3_page(1:3)), totals = 4 * 3600)
  expect_error(suppressMessages(report(cache = TRUE, cache_dir = dir)))
  expect_length(list.files(dir), 0)

  srv <- local_v3(list(v3_page(1:4)), totals = 4 * 3600)
  first <- suppressMessages(report(cache = TRUE, cache_dir = dir))
  again <- suppressMessages(report(cache = TRUE, cache_dir = dir))
  expect_equal(srv$n_search(), 1)
  expect_equal(first, again)
})

test_that("the default cache folder is a dedicated sub-folder, settable by option (#67)", {
  withr::local_options(togglr.cache_dir = NULL)
  expect_equal(toggl_cache_dir(), file.path(rappdirs::user_cache_dir("togglr"), "reports"))
  dir <- withr::local_tempdir()
  withr::local_options(togglr.cache_dir = dir)
  local_v3(list(v3_page(1:4)), totals = 4 * 3600)
  suppressMessages(report(cache = TRUE))
  expect_gt(length(list.files(dir)), 0)
})

test_that("memoise_cache_dir still works, with a deprecation warning (#67)", {
  dir <- withr::local_tempdir()
  local_v3(list(v3_page(1:4)), totals = 4 * 3600)
  expect_warning(suppressMessages(report(cache = TRUE, memoise_cache_dir = dir)), "memoise_cache_dir.*deprecated")
  expect_gt(length(list.files(dir)), 0)
})

test_that("clean_cache() empties the reports cache; clean_memoise_cache() is deprecated (#67)", {
  dir <- withr::local_tempdir()
  writeLines("x", file.path(dir, "a"))
  clean_cache(cache_dir = dir)
  expect_false(dir.exists(dir))
  old <- withr::local_tempdir()
  expect_warning(clean_memoise_cache(memoise_cache_dir = old), "deprecated.*clean_cache")
})

test_that("the cache is kept per token and per user filter (revue #75)", {
  dir <- withr::local_tempdir()
  srv <- local_v3(list(v3_page(1:4), v3_page(1:2), v3_page(1)), totals = c(4, 4, 2, 2, 1, 1) * 3600)
  get <- function(token, users = NULL) suppressMessages(get_detailled_report(
    api_token = token, workspace_id = 1, since = as.Date("2026-09-01"), until = as.Date("2026-09-30"),
    cache = TRUE, cache_dir = dir, user_ids = users))
  expect_equal(nrow(get("admin")), 4)
  expect_equal(nrow(get("member")), 2)
  expect_equal(nrow(get("admin", users = 7)), 1)
  expect_equal(nrow(get("admin")), 4) # depuis le cache
  expect_equal(srv$n_search(), 3)
})
