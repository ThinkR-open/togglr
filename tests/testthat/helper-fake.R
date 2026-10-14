# Build an httr response without network
fake_response <- function(status = 200, body = list(), headers = list()) {
  headers <- c(list(`content-type` = "application/json; charset=utf-8"), headers)
  structure(
    list(
      url = "https://api.track.toggl.com/fake",
      status_code = status,
      headers = structure(headers, class = c("insensitive", "list")),
      content = charToRaw(as.character(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null")))
    ),
    class = "response"
  )
}

# A queue of responses served one by one by the mocked toggl_http_get()
serve <- function(...) {
  queue <- list(...)
  calls <- 0
  list(
    get = function(url, api_token) {
      calls <<- calls + 1
      if (calls > length(queue)) stop("unexpected extra call: ", url)
      queue[[calls]]
    },
    calls = function() calls
  )
}

# One page of the reports API v2 /details
report_page <- function(ids, total, per_page = 50, total_grand = NULL) {
  list(
    total_count = total,
    total_grand = total_grand,
    per_page = per_page,
    data = lapply(ids, function(i) list(
      id = i, pid = 1L, uid = 2L, description = paste("entry", i),
      start = "2026-10-12T09:00:00+02:00", end = "2026-10-12T10:00:00+02:00",
      updated = "2026-10-12T10:00:00+02:00", dur = 3600000L, user = "Someone",
      project = "presta_x_"
    ))
  )
}
