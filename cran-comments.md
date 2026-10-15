## R CMD check results

0 errors | 0 warnings | 0 notes

* `R CMD check --as-cran` on R 4.5.1 (Ubuntu), with and without `NOT_CRAN`.
* GitHub Actions, workflow R-CMD-check of r-lib/actions: macOS (release),
  Windows (release), Ubuntu (release, devel, oldrel-1).
* win-builder, 2026-10-15: Status OK on R 4.6.1 (2026-06-24 ucrt) and on
  R Under development (unstable) (2026-10-14 r90673 ucrt), without any NOTE.

## Tests

* The tests never reach the network: every HTTP call is replaced by a double
  (`tests/testthat/setup-no_network.R`).

## Changes since the CRAN version

* CRAN has togglr 0.2.1 (2024). The 0.3.0 section of NEWS.md was a GitHub
  release only; everything since 0.2.1 is new on CRAN with 1.0.0.
* This is a major release. The breaking changes are listed in NEWS.md under
  "Breaking changes": Reports API v3 replaces Reports API v2, which Toggl has
  shut down; `create_client()` and `toggl_update_entries()` return the created
  or updated objects instead of httr responses; refused writes raise an error.

## Reverse dependencies

* None on CRAN.
