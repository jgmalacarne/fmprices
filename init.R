# init.R
#
# Install packages if not already installed
#
my_packages = c("dplyr",
                "ggplot2",
                "gt",
                "htmltools",
                "htmlwidgets",
                "reactable",
                "scales",
                "shiny",
                "shinyalert",
                "shinyBS",
                "shinydashboard",
                "shinydashboardPlus",
                "shinyWidgets",
                "shinyvalidate")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
}
invisible(sapply(my_packages, install_if_missing))

