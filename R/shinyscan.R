#' A Shiny app for scan
#'
#' Run a Shiny app with most of the scan functions.
#'
#' @details This function launches a shiny application in your default browser.
#'   You need to have the the `scplot` and `shiny` installed.
#'   These packages are suggested but not necessarily installed along with scan.
#'   `shinyscan()` will ask you to install missing packages.
#'
#' @export
shinyscan <- function() {
  
  miss <- c()
  
  if (!requireNamespace("scplot", quietly = TRUE)) {
    miss <- c(miss, "scplot")
  }
  if (!requireNamespace("shiny", quietly = TRUE)) {
    miss <- c(miss, "shiny")
  }
  
  if (length(miss > 0)) {
    cat("shinyscan needs the following additional packages to run: \n", 
        paste0(miss, collapse = ", "), "\n")
    res <- readline("Enter `y` to install the packages: ")
    if (res %in% c("y", "Y")) {
      install.packages(miss)
    } else {
      stop("Packages missing")
    }
  }
  
  shiny::runApp(system.file('shiny', package = 'scan'),launch.browser = TRUE)
  
}