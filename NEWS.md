# togglr (development version, toward 1.0.0)

* The API token is no longer printed: `create_client()` and
  `toggl_create_project()` used `httr::verbose()`, which shows the
  `Authorization` header (#50).
* Every request times out after `options(togglr.timeout)` seconds (120 by
  default), and network errors (timeout, DNS, connection reset) are retried
  like a 5xx (#45).
* `get_detailled_report()` returns fixed column types (e.g. `task` is always
  text, `tid` integer), whatever the content of the page: periods over a year
  with an empty year no longer fail to combine (#46).
* `get_detailled_report()` also compares the total duration (`total_grand`)
  returned with each page: a deletion followed by an insertion during the
  pagination, which leaves `total_count` unchanged, now triggers a new fetch
  instead of returning a stale entry and missing another (#47).
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
* `get_detailled_report_paged()` has the same default period as
  `get_detailled_report()` (the last year, until today): it was three years,
  beyond the one year limit of the reports API v2, until tomorrow (#63).
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

# togglr 0.3.0

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
