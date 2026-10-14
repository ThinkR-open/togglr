# Garde-fou : aucun test ne doit atteindre l'API Toggl. Chaque test qui a
# besoin d'une reponse la fournit explicitement avec local_mocked_bindings().
no_network <- function(...) stop("network access attempted in a test", call. = FALSE)
local_mocked_bindings(
  GET = no_network, POST = no_network, PUT = no_network, PATCH = no_network, DELETE = no_network,
  .env = testthat::teardown_env()
)

# Pas d'espacement par defaut dans les tests (ceux qui le testent le fixent)
withr::local_options(togglr.min_interval = 0, .local_envir = testthat::teardown_env())
