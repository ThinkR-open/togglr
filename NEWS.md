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
