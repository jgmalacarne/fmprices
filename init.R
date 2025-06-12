# init.R
#
# Install packages if not already installed
#
my_packages = c(
  "htmltools",
  "htmlwidgets",
  "reactable",
  "scales",
  "shiny",
  "shinydashboard",
  "shinydasboardPlus",
  "shinyWidgets",
  "tidyverse")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
}
invisible(sapply(my_packages, install_if_missing))

