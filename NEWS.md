# togglr 1.0.0

## Breaking changes

* Reports come from the Reports API v3 (#76): `get_summary_report()` items no
  longer give `cur`, `sum` and `rate` (`NA`); entries without a visible project
  are grouped under an `NA` id and name instead of "Private Project";
  the `user_agent` argument of the report functions is ignored. The columns
  and types of `get_detailled_report()` are unchanged. The rounding set in the
  Toggl profile is never applied (`rounding = 0` in every request), as in
  Reports v2.
* `get_workspace_id()` returns the default workspace of the account, no longer
  always the first one (#23).
* `get_time_entries()` fails before any request for a period starting more
  than 3 months ago (the API refused it anyway) (#13).
* `create_client()` returns the created client (a list), no longer the httr
  response (#54); `toggl_update_entries()` sends one PUT per entry and returns
  the updated entry, or a list of them (#56).
* A write refused by the API raises an error (#54); `toggl_start()` and
  `toggl_create()` return `NA`, with a warning, when the API accepted the
  entry but its answer could not be read.
* `get_toggl_api_token()` fails, outside an interactive session, instead of
  prompting, and never deletes the stored key (#51).
* `set_toggl_api_token()` returns the token invisibly (#84).
* `simplify()` is rewritten on the public API of jsonlite (#69).
* `get_detailled_report()`: `cache = FALSE` by default, `max_page` and
  `memoise_cache_dir` deprecated (#60, #67); a 7th positional argument now
  goes to `cache`.

## Other changes

* `get_detailled_report()` reads Reports API v3 (`/search/time_entries`,
  enriched with the names of users, projects, clients and tags) instead of
  Reports v2, with the same columns and types. Pages of 1000 entries
  (`options(togglr.page_size)`) instead of 50: a month of a team of 20 is read
  in one or two requests. The period is checked against the total duration of
  `/search/time_entries/totals`, equal before and after the pagination and,
  up to the rounding of the API (1 s per entry), to the sum of the entries.
  An entry received that was edited after the read started is a movement too:
  the pages are numbered by row, so a deletion followed by an insertion of the
  same duration shifts the rows without changing the totals. This replaces the
  `total_grand` tolerance for running timers, which could hide such a movement
  (#76, #77). A running timer may only grow by the time between the two
  totals, measured by the clock of the server from its own start, not by the
  time spent waiting before the first total. One movement still escapes: an
  entry deleted and replaced by one of exactly the same duration that is not in
  the pages received.
  New argument `user_ids`. `user_agent` is ignored.
* `get_summary_report()`, `get_weekly_report()` and `get_dashboard()` read
  Reports API v3 too, with the output of Reports v2. Reports v3 only returns
  ids: the names of users, projects and clients come from the Track API v9,
  archived projects included. `get_weekly_report()` returns an empty tibble
  for a week without any entry. An unreadable answer to a report request is an
  error, never an empty report.
  The amounts of the summary items (`cur`, `sum`, `rate`) are no longer given
  (NA). Entries without a visible project are grouped with an `NA` id and name
  (Reports v2 called them "Private Project") (#76).
* With a shared state file, an exhausted quota, or a spread of the requests,
  recorded by another process while this one was waiting is no longer erased:
  it is waited for in turn, or fails within `max_wait` (#78).
* The requests of Reports v3 (`/reports/api/v3/workspace/<id>/`) count against
  the quota of their workspace, like the requests of the API v9
  (`/workspaces/<id>/`): an exhausted Reports quota no longer blocks `/me`.
* `get_time_entries()` is never truncated: `/me/time_entries` returns at most
  1000 entries, so a full answer splits the period in two, recursively. A
  period starting more than 3 months ago (the limit of the API) fails before
  any request, pointing to `get_detailled_report(user_ids = )` (#13). Its
  output gains a `client_name` column; `project_name` and `client_name` cover
  the archived projects too (#14). `get_project_id_and_name()` gains `active`
  (`NULL`, `"false"` or `"both"`) and returns 0 rows, not an error, for a
  workspace without any matching project. `since` and `until` may be text
  dates again.
* `get_workspace_id()` returns the default workspace of the account
  (`default_workspace_id` of `/me`), and the first workspace only when no
  default is set; it used to be always the first one. New `get_workspaces()`
  lists every workspace, the default one flagged (#23).
* `get_dashboard()` and `get_all_project_names()` gain `user_ids`, to keep the
  projects some team members worked on (#12).
* New `get_project_data()`: the data of one project from its id (#28).
* `toggl_start(tags = )` sends its tags, now covered by a test (#16).
* `set_toggl_api_token()` returns the token invisibly (it was printed when
  called at top level, e.g. in a script whose output is logged), and checks
  the new token before deleting the stored one. `update_toggl_api_token()`
  asks first: a cancelled input keeps the stored token (#84).
* Tests: coverage from 85 % to 91 %, a real multi-process test of
  `togglr.state_file`, no test depending on covr's instrumentation; new
  workflow measuring the coverage (#84, #32, #33).
* DESCRIPTION declares `R (>= 4.1.0)` and the minimal versions of its
  dependencies; the obsolete `inst/dependencies.R` is no longer shipped (#83).

* The API token is no longer printed: `create_client()` and
  `toggl_create_project()` used `httr::verbose()`, which shows the
  `Authorization` header (#50).
* Every request times out after `options(togglr.timeout)` seconds (120 by
  default), and network errors (timeout, DNS, connection reset) are retried
  like a 5xx (#45).
* `get_detailled_report()` returns fixed column types (e.g. `task` is always
  text, `tid` integer), whatever the content of the page: periods over a year
  with an empty year no longer fail to combine (#46).
* New option `togglr.state_file`: the pacing of the reports calls and the
  knowledge of an exhausted quota are stored in that file, under a lock, and
  shared by every R process using it (callr / future / targets workers, cron
  jobs). Without it, the state stays private to the session (#48).
* A 402 without quota headers (a feature of a paid plan) now stops at once with
  the message of the API, instead of blocking every request for an hour. An
  exhausted quota is remembered per token, as a hash: the token is never
  stored (#49).
* The `api_token` and `workspace_id` given to a function are now used by every
  call it makes: project, client and dashboard helpers, `get_time_entries()`
  (new `workspace_id` argument), `toggl_start()`, `toggl_stop()`,
  `toggl_create()`, `toggl_create_project()` no longer fall back to the keyring
  token nor to an extra `/workspaces` request. `client_name_to_id()` and
  `client_id_to_name()` gain a `workspace_id` argument (#51).
* `get_toggl_api_token()` no longer deletes the stored key when the keyring
  cannot be read (e.g. locked under cron); in a non interactive session it
  fails with a clear message instead of prompting (#51).
* `toggl_start()` and `toggl_create()` send the start time converted to UTC,
  instead of removing one hour: entries were one hour off in summer, and more
  outside Paris (#52, from #44 by @bradyrippon).
* `get_current_duration()` counts from the start of the running entry, not
  from its last edit (`at`); `get_project_total()` and
  `get_current_project_task_detail()` were affected (#53).
* Every write (POST, PUT, PATCH) goes through `toggl_request()`, with the
  policy of the reads: an error of the API is raised with its message (no more
  "$ operator is invalid for atomic vectors"), the quota and 429 are respected.
  A POST is never retried after a 5xx or a network error, as it may have been
  processed. `toggl_start()` / `toggl_create()` notify then raise the error;
  `toggl_stop()` only notifies STOP once the API accepted it. `create_client()`
  now returns the created client (a list) instead of the raw httr response
  (#54).
* `toggl_create_project()` works again: no crash without client, an existing
  client is reused instead of being created again, its `client_id` is sent,
  and the body follows the v9 format (`private`, `color`, `active` are no
  longer ignored) (#55).
* `toggl_update_entries()` sends one PUT per entry (the v9 API takes a single
  id per PUT), sends `stop` (it was accepted then dropped), sends the project as
  `project_id`, and no longer sends the read-only `at`. It returns the updated
  entry, or a list of them for several ids (#56).
* `get_time_entries()` sends its dates in UTC (`...Z`): the `+` of a time zone
  offset was sent unencoded in the url, and read as a space by the server
  (#57). Entries with several tags are covered by a test (#30).
* `get_detailled_report()` splits by calendar year as soon as a period spans
  365 days or more across a new year: a 367 days period (bounds included) was
  sent as is, beyond the one year limit of the reports API v2 (#58).
* The pagination of `get_all_projects()` is bounded: it stops with an error if
  the API returns the same page twice, or after `options(togglr.max_pages)`
  pages (100 by default), instead of spending the quota in an endless loop
  (#59).
* `get_detailled_report(max_page = )` now warns that the argument is
  deprecated and ignored (#60).
* The reports functions (`get_detailled_report()`, `get_summary_report()`,
  `get_weekly_report()`, `get_dashboard()`) accept `since` / `until` as a
  `Date`, a date-time (its day in its own time zone) or a text "AAAA-MM-JJ",
  and fail with a clear message, before any request, on anything else or on
  `since` after `until` (#61).
* Default periods ("one year ago") are computed with `%m-%`: they were `NA`
  on February 29th (#62).
* `options(togglr.min_interval)` (1 s by default) now spaces every request,
  reads and writes, retries included, not only the pages of the detailled
  report (#64).
* A period that keeps moving during the pagination is read up to
  `options(togglr.report_attempts)` times (3 by default, 2 before) (#65).
* An entry seen twice during the pagination keeps its most recently updated
  version, not the first one read (#66).
* The reports cache (`cache = TRUE`) lives in its own folder, the `reports`
  sub-folder of `rappdirs::user_cache_dir("togglr")` or
  `options(togglr.cache_dir)`, apart from the memoise files of 0.2.x. New
  argument `cache_dir` and function `clean_cache()`; `memoise_cache_dir` and
  `clean_memoise_cache()` are deprecated (#67).
* The quota announced by every answer (`x-toggl-quota-remaining`) is used: at
  0, the next request waits for the reset (or fails within `max_wait`) instead
  of receiving a 402; with fewer than `options(togglr.quota_spread)` (10)
  requests left, they are spread until the reset (#68).
* `simplify()` no longer calls an internal function of jsonlite
  (`getFromNamespace()`): it uses the public `toJSON()` / `fromJSON()`, at full
  precision, with the same result (#69).
* `globalVariables()` no longer declares function names (`case_when`,
  `full_join`, `tribble`) nor unused names, which could hide a missing
  import (#70).
* Every request sends the user agent `togglr/<version>` in its header;
  `get_dashboard()` no longer sends a hardcoded `api_test` (#71). The
  `user_agent` argument of the report functions is kept for compatibility and
  ignored.
* `get_workspace_id()` fails with its message when the account has no
  workspace, instead of "subscript out of bounds". New tests cover the edge
  cases of the pagination, of the quota and of the cache (#73).
* Package metadata and documentation: new title and description, no more
  `LazyData` without data, targeted imports only (no more whole `dplyr`,
  `httr`, `keyring`, `assertthat`, `getPass`), `get_detailled_report()` has
  its own help page, the vignette installs from GitHub and no longer runs
  `install.packages()` during `R CMD check`. `R CMD check` is clean: 0 error,
  0 warning, 0 note (#74).
* `toggl_create_project()` (and so `toggl_start()` on a new project) works
  again in a workspace without any client: `get_all_client_info()` returns
  the `id` and `name` columns, even with no rows (review of #75).
* The reports cache is kept per token (as a hash): a period fetched with one
  token is no longer returned to another token of the same workspace (review of #75).
* The spreading of the last requests of the quota waits at most `max_wait`,
  with a message, and only slows down the token that is short of quota (review of #75).
* The quota is remembered per token and per scope (the workspace or
  organization of the url, or the user for `/me` and `/workspaces`): an
  exhausted workspace no longer blocks `/me` or another workspace (review of #75).
* A request processed by the API is returned even if its quota cannot be
  recorded in the state file (warning): a time entry is no longer reported
  as not started, then created twice (review of #75).
* One pass on the state file per request instead of three, and a read no
  longer rewrites it (review of #75).
* A time entry accepted by the API but whose answer cannot be read is
  reported as started (with a warning, id `NA`), not as `NOT STARTED` (review of #75).
* A state file that cannot be replaced is an error instead of a silently
  lost update, and the temporary file is removed (review of #75).
* Network errors are no longer retried when the backoff exceeds `max_wait` (review of #75).

# togglr 0.3.0 (GitHub only, never released on CRAN)

* Every read of the API goes through `toggl_GET()`: HTTP errors are no longer
  turned into empty results. 429 and 5xx are retried, waiting what the API asks
  (within `options(togglr.max_wait)`, 60 s by default), any other error stops.
* `get_detailled_report()` no longer returns silently truncated periods: the
  number of pages comes from `total_count`, entries are de-duplicated by `id`,
  the period is fetched again if entries move during pagination, and an error is
  raised if the result is still incomplete.
* `get_detailled_report()` no longer memoises pages. The new `cache` argument
  (default `FALSE`) stores a whole period, only once it is complete and checked;
  reserve it to closed periods. `max_page` is deprecated and ignored.
* Quota exhaustion (HTTP 402) is remembered for the session: no request is
  sent before the reset (waited for within `max_wait`, otherwise an error).
  A 429 without `retry-after` backs off for a minute.
* Calls to the reports API are spaced by at least one second, or by
  `options(togglr.min_interval)` seconds to spread a long extraction.

# togglr 0.1.99.9002

* from api V8 to V9

# togglr 0.1.34.9000

* `get_time_entries()` can now handle multiple tags, stored in a list column (#21, @pat-s)


# togglr 0.1.34

* Added pkgdown documentation
* Added a `NEWS.md` file to track changes to the package.
