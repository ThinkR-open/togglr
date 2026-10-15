# ---- #76 : rapport detaille via Reports API v3 ----------------------------------------

local_v3 <- function(pages, totals, server_dates = NULL, env = parent.frame()) {
  srv <- serve_v3(pages, totals, server_dates = server_dates)
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
  expect_equal(b$rounding, 0)
  totals <- Filter(function(x) grepl("/totals$", x$.url), srv$bodies())
  expect_length(totals, 2)
  expect_equal(totals[[1]]$rounding, 0)
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
  # first_id / first_timestamp font renvoyer une page vide par l'API
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
  expect_equal(nrow(out), 4)
})

test_that("an entry edited after the start of the read triggers a new read, even with equal totals (#77, revue #85)", {
  # l'entree 3 (60 min) a ete supprimee et l'entree 5 (60 min aussi) creee entre le
  # premier total et la page : les totaux ne bougent pas, mais `at` de 5 est posterieur
  # au debut de la lecture. L'horloge avance de 5 min a chaque consultation.
  t0 <- as.POSIXct("2026-10-12 12:00:00", tz = "UTC")
  n <- 0
  local_mocked_bindings(toggl_now = function() {
    n <<- n + 1
    t0 + 300 * (n - 1)
  })
  page <- structure(c(v3_page(c(1, 2, 4)), list(v3_row(5, at_tz = "2026-10-12T14:02:00+02:00"))),
                    more = FALSE)
  srv <- local_v3(list(page, page), totals = 4 * 3600)
  out <- suppressMessages(report())
  expect_setequal(out$id, c(1, 2, 4, 5))
  expect_equal(nrow(out), 4)
  expect_equal(srv$n_search(), 2)
})

test_that("the start of the read is the Date of the server, not the local clock (revue #85)", {
  # horloge locale en retard de 90 s sur le serveur ; une entree modifiee 30 s avant
  # l'appel (heure serveur) n'est pas un mouvement
  local_mocked_bindings(toggl_now = function() as.POSIXct("2026-10-12 11:58:30", tz = "UTC"))
  page <- structure(list(v3_row(5, at_tz = "2026-10-12T13:59:30+02:00")), more = FALSE)
  srv <- local_v3(list(page), totals = 3600,
                  server_dates = c("Mon, 12 Oct 2026 12:00:00 GMT", "Mon, 12 Oct 2026 12:00:03 GMT"))
  out <- suppressMessages(report())
  expect_equal(out$id, 5)
  expect_equal(srv$n_search(), 1)
})

test_that("entries still edited during the last read raise an error (#77, revue #85)", {
  withr::local_options(togglr.report_attempts = 2)
  local_mocked_bindings(toggl_now = function() as.POSIXct("2026-10-12 12:00:00", tz = "UTC"))
  page <- structure(list(v3_row(5, at_tz = "2026-10-12T14:02:00+02:00")), more = FALSE)
  local_v3(list(page, page), totals = 3600)
  expect_error(suppressMessages(report()), "edited during the read")
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
  # timer demarre a 08:00Z ; totaux a 08:10:00Z (600 s) et 08:10:01Z (601 s)
  running <- v3_row(9, seconds = 600, stop = NULL, start = "2026-10-12T10:00:00+02:00",
                    at_tz = "2026-10-12T10:00:00+02:00")
  page <- structure(c(v3_page(1:2), list(running)), more = FALSE)
  srv <- local_v3(list(page), totals = c(2 * 3600 + 600, 2 * 3600 + 601),
                  server_dates = c("Mon, 12 Oct 2026 08:10:00 GMT", "Mon, 12 Oct 2026 08:10:01 GMT"))
  out <- suppressMessages(report())
  expect_equal(out$id, 1:2)
})

test_that("the growth of a running timer is measured between the two totals, by the server clock (revue Copilot #85)", {
  # 2 entrees de 3600 s recues ; une entree de 3600 s supprimee et une de 3609 s inseree
  # jamais recue ; timer en cours demarre a 11:49:44Z ; totaux a 12:00:00Z (7800) et
  # 12:00:32Z (7841 = 7200 + 3609 - 3600 + 648 - 616) ; l'horloge locale a attendu 16 s
  # avant le premier total puis 32 s : la croissance admise ne doit pas inclure l'attente
  n <- 0
  local_mocked_bindings(toggl_now = function() {
    n <<- n + 1
    as.POSIXct("2026-10-12 12:00:00", tz = "UTC") + 48 * (n - 1)
  })
  running <- v3_row(9, seconds = 616, stop = NULL, start = "2026-10-12T13:49:44+02:00",
                    at_tz = "2026-10-12T13:49:44+02:00")
  page <- structure(c(v3_page(1:2), list(running)), more = FALSE)
  local_v3(list(page, page, page), totals = rep(c(7800, 7841), 3),
           server_dates = rep(c("Mon, 12 Oct 2026 12:00:00 GMT", "Mon, 12 Oct 2026 12:00:32 GMT"), 3))
  expect_error(suppressMessages(report()), "incomplete toggl detailled report")
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
