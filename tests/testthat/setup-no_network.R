# Garde-fou : aucun test ne doit atteindre l'API Toggl. Chaque test qui a
# besoin d'une reponse la fournit explicitement avec local_mocked_bindings().
no_network <- function(...) stop("network access attempted in a test", call. = FALSE)
local_mocked_bindings(
  GET = no_network, POST = no_network, PUT = no_network, PATCH = no_network, DELETE = no_network,
  .env = testthat::teardown_env()
)
