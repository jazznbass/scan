#' A Shiny app for scan
#'
#' Run a Shiny app with most of the scan functions.
#'
#' @param scdf If you provide an *scdf* here, it will be loaded at startup.
#' @param quiet If TRUE (default) does not report shiny messages in the console.
#' @param browser c("external","viewer") 
#' @param \dots Further arguments passed to the `shiny::runApp()` function.
#' @details This function launches a shiny application.
#'   You need to have `scplot` and `shiny` installed.
#'   These packages are suggested but not necessarily installed along with scan.
#'   `shinyscan()` will ask to install missing packages.
#'
#' @export
shinyscan <- function(scdf = NULL,
                      quiet = TRUE, 
                      browser = c("external", "viewer"), 
                      ...) {
  
  browser <- browser[1]
  
  miss <- c()
  if (!requireNamespace("scplot", quietly = TRUE)) miss <- c(miss, "scplot")
  if (!requireNamespace("shiny",  quietly = TRUE)) miss <- c(miss,  "shiny")
  
  if (length(miss) > 0) {
    message("shinyscan needs: ", paste(miss, collapse = ", "))
    ans <- utils::askYesNo("Install missing packages now?")
    if (isTRUE(ans)) {
      install.packages(miss) 
    } else {
      stop("Packages missing.", call. = FALSE)
    }
  }
  
  browser <- match.arg(browser)
  
  old_opt <- if (identical(browser, "external")) {
    options(shiny.launch.browser = TRUE)
  } else {
    options(shiny.launch.browser = FALSE)
  }

  if (inherits(scdf, "scdf")) {
    old_opt <- options(scan.shinyscan.initial = scdf)
  }
  
  on.exit(options(old_opt), add = TRUE)
  
  app_dir <- system.file("shiny_scan", package = "scan")
  if (app_dir == "") 
    stop("Cannot find inst/shiny_scan in installed package.", call. = FALSE)
  
  shiny::runApp(app_dir, quiet = quiet, ...)
  
}