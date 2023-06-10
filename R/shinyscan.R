#' A Shiny app for scan
#'
#' Run a Shiny app with most of the scan functions.
#'
#' @param quiet If TRUE (default) does not report shiny messages in the console.
#' @param \dots Further arguments passed to the `shiny::runApp()` function.
#' @details This function launches a shiny application.
#'   You need to have `scplot` and `shiny` installed.
#'   These packages are suggested but not necessarily installed along with scan.
#'   `shinyscan()` will ask to install missing packages.
#'
#' @export
shinyscan <- function(quiet = TRUE, ...) {
  
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
    cat("(if you encounter problems with the installation, restart R and try again to install the packages.")
    if (res %in% c("y", "Y")) {
      install.packages(miss)
    } else {
      stop("Packages missing")
    }
  }
  
  #prev <- getOption("shiny.launch.browser")
  
  #if (mode == "browser") 
  ##  options(shiny.launch.browser = .rs.invokeShinyWindowExternal)
  #if (mode == "window") 
  #  options(shiny.launch.browser = .rs.invokeShinyWindowViewer)
  #if (mode == "viewer") 
  #  options(shiny.launch.browser = .rs.invokeShinyPaneViewer)
  
  
  shiny::runApp(
    system.file('shiny', package = 'scan'),
    quiet = quiet,
    ...
  )
  
  #options(shiny.launch.browser = prev)
  
}