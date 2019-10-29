usethis::use_build_ignore("dev_history.R")
usethis::use_readme_rmd()
usethis::use_news_md()

# Documentation
attachment::att_to_description(extra.suggests = "pkgdown")
devtools::check()

# pkgdown
# _Pkgdown
usethis::use_pkgdown()

