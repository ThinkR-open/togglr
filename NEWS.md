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
