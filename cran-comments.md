## R CMD check results

0 errors | 0 warnings | 0 notes

* Checked locally with `--as-cran` on R 4.5.1 (Linux), and on GitHub Actions
  (macOS, Windows, Ubuntu release / devel / oldrel-1).
* Checked on win-builder (R release and R devel).

## Tests

* The tests never reach the network: every HTTP call is mocked
  (`tests/testthat/setup-no_network.R`).

## Breaking changes

* This is a major release. The breaking changes are listed in NEWS.md, under
  "Breaking changes": Reports API v3 replaces Reports API v2, which Toggl has
  shut down; `create_client()` and `toggl_update_entries()` return the created
  or updated objects instead of httr responses; refused writes raise an error.
