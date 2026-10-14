# enleve les faux positifs du check
globalVariables(
  unique(c(
    ".",
    "name",
    "project",
    "time",
    "client_id", "client_name",
    "id","start","duration","pretty_duration",'project_name','description','pid','wid',
    "title", "id", "fullname", "time"
  ))
)