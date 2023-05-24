#' A Shiny app for scan
#'
#' Run a Shiny app with most of the scan functions.
#'
#' @details This function launches a shiny application in your default browser.
#'   You need to have the following CRAN packages installed: `scplot`, `shiny`.
#'   `shinyjs`, `markdown`. These packages are suggested but not necessarily
#'   installed along with cran. `shinyscan()` will tell you if a package is
#'   missing.
#'
#'
#' @export
shinyscan <- function() {
  
  if (!requireNamespace("markdown", quietly = TRUE)) {
    stop("You need to install the 'markdown' package to run this app ",
            "with install.packages('markdown')")
  }
  if (!requireNamespace("scplot", quietly = TRUE)) {
    stop("You need to install the 'scplot' package to run this app ",
            "with install.packages('scplot')")
  }
  if (!requireNamespace("shinyjs", quietly = TRUE)) {
    stop("You need to install the 'shinyjs' package to run this app ",
            "with install.packages('shinyjs')")
  }
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("You need to install the 'shiny' package to run this app ",
            "with install.packages('shiny')")
  }
  
  shiny::runApp(system.file('shiny', package = 'scan'),launch.browser = TRUE)
  
}