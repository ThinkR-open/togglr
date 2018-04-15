to_install <- c("assertthat","dplyr","getPass","glue","httr","jsonlite","keyring","lubridate","magrittr","parsedate","prettyunits","purrr","rstudioapi")
for (i in to_install) {
  message(paste("looking for ", i))
  if (!requireNamespace(i)) {
    message(paste("     installing", i))
    install.packages(i)
  }

}