# ---- #76 : rapport detaille via Reports API v3 ----------------------------------------

local_v3 <- function(pages, totals, env = parent.frame()) {
  srv <- serve_v3(pages, totals)
  local_mocked_bindings(POST = srv$send, toggl_sleep = function(s) NULL, .env = env)
  local_fresh_state(env)
  srv
}

test_that("the detailled report is read from Reports v3, with the v2 columns and types (#76)", {
  srv <- local_v3(list(v3_page(1:2, tags = list("a", "b"), billable_cents = 12345L, billable = TRUE)),
                  totals = 7200)
  out <- suppressMessages(report())
  expect_match(srv$searches()[[1]]$.url,
               "^https://api.track.toggl.com/reports/api/v3/workspace/1/search/time_entries$")
  expect_named(out, names(detailled_report_types()))
  expect_equal(out$id, c(1, 2))
  expect_equal(out$uid, c(2L, 2L))
  expect_equal(out$pid, c(1L, 1L))
  expect_true(all(is.na(out$tid)))
  expect_equal(out$start, rep("2026-10-12T09:00:00+02:00", 2))
  expect_equal(out$end, rep("2026-10-12T10:00:00+02:00", 2))
  expect_equal(out$updated, rep("2026-10-12T10:00:00+02:00", 2))
  expect_equal(out$dur, c(3600000, 3600000))
  expect_equal(out$user, c("user2", "user2"))
  expect_equal(out$project, c("presta_x", "presta_x"))
  expect_equal(out$client, c("acme", "acme"))
  expect_equal(out$project_hex_color, c("#06aaf5", "#06aaf5"))
  expect_equal(out$billable, c(123.45, 123.45))
  expect_equal(out$is_billable, c(TRUE, TRUE))
  expect_equal(out$cur, c("EUR", "EUR"))
  expect_false(any(out$use_stop))
  expect_equal(out$tags[[1]], c("a", "b"))
})

test_that("the request asks the whole period, end included, enriched, with large pages (#76)", {
  srv <- local_v3(list(v3_page(1)), totals = 3600)
  suppressMessages(report())
  b <- srv$searches()[[1]]
  expect_equal(b$start_date, "2026-10-01")
  expect_equal(b$end_date, "2026-10-31")
  expect_true(b$enrich_response)
  expect_equal(b$page_size, 1000L)
  expect_null(b$first_id)
})

test_that("togglr.page_size sets the page size (#76)", {
  withr::local_options(togglr.page_size = 200)
  srv <- local_v3(list(v3_page(1)), totals = 3600)
  suppressMessages(report())
  expect_equal(srv$searches()[[1]]$page_size, 200L)
})

test_that("pages are followed with X-Next-Row-Number until there is no next page (#76)", {
  srv <- local_v3(list(v3_page(1:2, more = TRUE), v3_page(3:4, more = TRUE), v3_page(5)),
                  totals = 5 * 3600)
  out <- suppressMessages(report())
  expect_equal(out$id, 1:5)
  s <- srv$searches()
  expect_length(s, 3)
  expect_equal(s[[2]]$first_row_number, 3)
  expect_equal(s[[3]]$first_row_number, 5)
  # first_id / first_timestamp font renvoyer une page vide par l'API (constate le 14/10/2026)
  expect_null(s[[2]]$first_id)
  expect_null(s[[2]]$first_timestamp)
})

test_that("an empty period gives the empty structure (#76)", {
  local_v3(list(structure(list(), more = FALSE)), totals = 0)
  out <- suppressMessages(report())
  expect_equal(nrow(out), 0)
  expect_named(out, names(detailled_report_types()))
})

test_that("entries missing compared to the totals trigger a new read (#76)", {
  srv <- local_v3(list(v3_page(1:2), v3_page(1:3)), totals = 3 * 3600)
  out <- suppressMessages(report())
  expect_equal(out$id, 1:3)
  expect_equal(srv$n_search(), 2)
})

test_that("totals changing during the pagination trigger a new read (#76)", {
  # avant / apres la 1re lecture : 3 h puis 4 h ; 2e lecture : 4 h stable
  srv <- local_v3(list(v3_page(1:3), v3_page(1:4)), totals = c(3, 4, 4, 4) * 3600)
  out <- suppressMessages(report())
  expect_equal(out$id, 1:4)
})

test_that("a deletion + an insertion of close durations is detected by the totals (#76, #77)", {
  # la page lue contient l'entree 3 (60 min) supprimee ; l'entree 5 (60 min 9 s) l'a remplacee
  lue <- v3_page(c(1, 2, 3, 4))
  vraie <- c(v3_page(c(1, 2, 4)), list(v3_row(5, seconds = 3609)))
  srv <- local_v3(list(lue, structure(vraie, more = FALSE)),
                  totals = c(4 * 3600, 3 * 3600 + 3609, 3 * 3600 + 3609, 3 * 3600 + 3609))
  out <- suppressMessages(report())
  expect_setequal(out$id, c(1, 2, 4, 5))
})

test_that("a period still incomplete after the reads raises an error, never a truncation (#76)", {
  withr::local_options(togglr.report_attempts = 2)
  local_v3(list(v3_page(1:2), v3_page(1:2)), totals = 3 * 3600)
  expect_error(suppressMessages(report()), "incomplete toggl detailled report.*7200.*10800")
})

test_that("an entry seen twice keeps its most recently updated version (#76)", {
  p1 <- v3_page(1:2, more = TRUE)
  p2 <- list(v3_row(2, description = "edited", at_tz = "2026-10-12T09:30:00Z"), v3_row(3))
  srv <- local_v3(list(p1, structure(p2, more = FALSE)), totals = 3 * 3600)
  out <- suppressMessages(report())
  expect_equal(out$id, 1:3)
  expect_equal(out$description[out$id == 2], "edited")
})

test_that("a running entry is left out, and its growth does not count as an edit (#76)", {
  running <- v3_row(9, seconds = 600, stop = NULL)
  page <- structure(c(v3_page(1:2), list(running)), more = FALSE)
  srv <- local_v3(list(page), totals = c(2 * 3600 + 600, 2 * 3600 + 601))
  out <- suppressMessages(report())
  expect_equal(out$id, 1:2)
})

test_that("without a running entry, any change of the totals is an edit (#76)", {
  withr::local_options(togglr.report_attempts = 1)
  local_v3(list(v3_page(1:2)), totals = c(7200, 7201))
  expect_error(suppressMessages(report()), "incomplete")
})

test_that("user_ids are passed to the search and to the totals (#76)", {
  srv <- local_v3(list(v3_page(1)), totals = 3600)
  suppressMessages(get_detailled_report(api_token = "t", workspace_id = 1, since = "2026-10-01",
                                        until = "2026-10-31", user_ids = c(7, 8)))
  for (b in srv$bodies()) expect_equal(unlist(b$user_ids), c(7, 8))
})

test_that("a read-only search is retried after a 5xx (#76)", {
  calls <- 0
  srv <- serve_v3(list(v3_page(1)), totals = 3600)
  local_mocked_bindings(POST = function(url, ..., body = NULL) {
    calls <<- calls + 1
    if (calls == 2) return(fake_response(502)) # la 1re recherche
    srv$send(url, ..., body = body)
  }, toggl_sleep = function(s) NULL)
  local_fresh_state()
  expect_equal(suppressMessages(report())$id, 1)
})

test_that("an HTTP error during the pagination is an error, not a truncation (#76)", {
  n <- 0
  srv <- serve_v3(list(v3_page(1:2, more = TRUE), v3_page(3)), totals = 3 * 3600)
  local_mocked_bindings(POST = function(url, ..., body = NULL) {
    if (grepl("/search/time_entries$", url)) { n <<- n + 1; if (n == 2) return(fake_response(403, "forbidden")) }
    srv$send(url, ..., body = body)
  }, toggl_sleep = function(s) NULL)
  local_fresh_state()
  expect_error(suppressMessages(report()), "403")
})

test_that("the totals may differ from the sum by up to 1 s per entry (rounding of the API) (#76)", {
  # les totaux arrondissent la somme en ms, la recherche arrondit chaque entree
  srv <- local_v3(list(v3_page(1:3)), totals = 3 * 3600 + 2)
  expect_equal(nrow(suppressMessages(report())), 3)
})

test_that("a gap larger than 1 s per entry is a missing entry, never tolerated (#76)", {
  withr::local_options(togglr.report_attempts = 1)
  local_v3(list(v3_page(1:3)), totals = 3 * 3600 + 4)
  expect_error(suppressMessages(report()), "incomplete")
  local_v3(list(v3_page(1:3)), totals = 4 * 3600)
  expect_error(suppressMessages(report()), "incomplete")
})

test_that("the totals before and after the pagination must still be equal, to the second (#76)", {
  withr::local_options(togglr.report_attempts = 1)
  local_v3(list(v3_page(1:3)), totals = c(3 * 3600, 3 * 3600 + 1))
  expect_error(suppressMessages(report()), "incomplete")
})

test_that("an entry without project has project_color \"0\", as in Reports v2 (#76)", {
  r <- v3_row(1); r$project_id <- NULL; r$project_name <- NULL; r$project_color <- NULL
  r$project_hex <- NULL; r$client_name <- NULL
  local_v3(list(structure(list(r), more = FALSE)), totals = 3600)
  out <- suppressMessages(report())
  expect_equal(out$project_color, "0")
  expect_true(is.na(out$project))
  expect_true(is.na(out$pid))
})
