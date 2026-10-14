# enleve les faux positifs du check
globalVariables(
  unique(c(
    ".",
    "name",
    "project",
    "time",
    "client_id",
    "id","start","duration","pretty_duration",'project_name','description','pid','wid',
    # get_summary_report
    "total_currencies", "title", "id", "fullname", "time", "items"
  ))
)