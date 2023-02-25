#' A Shiny app for scan
#'
#' Run a Shiny app with most of the scan functions.
#' @export
scan_app <- function() {
  
  if (requireNamespace("shiny", quietly = TRUE)) {
    
    shiny::runApp(system.file('shiny', package = 'scan'),launch.browser = TRUE)
  } else {
    message("You need to install the 'shiny' package to run this app ",
            "with install.packages('shiny')")
  }
}