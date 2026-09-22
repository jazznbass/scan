#' A Shiny app for scan
#'
#' Run a Shiny app with most of the scan functions.
#' 
#' @return This function launches a Shiny application.
#' @author Juergen Wilbert
#' @param scdf If you provide an *scdf* here, it will be loaded at startup.
#' @param quiet If TRUE (default) does not report shiny messages in the console.
#' @param browser c("external","viewer") 
#' @param theme Bootstrap 5 theme. Default is "cerulean".
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
                      theme = "cerulean",
                      ...) {
  
  browser <- browser[1]

  miss <- c()
  if (!requireNamespace("scplot", quietly = TRUE)) miss <- c(miss, "scplot")
  if (!requireNamespace("shiny",  quietly = TRUE)) miss <- c(miss,  "shiny")
  
  if (length(miss) > 0) {
    notify("shinyscan needs: ", paste(miss, collapse = ", "))
    ans <- utils::askYesNo("Install missing packages now?")
    if (isTRUE(ans)) {
      install.packages(miss) 
    } else {
      abort("Packages missing.")
    }
  }
  
  browser <- match.arg(browser)
  
  app_options <- c(
    "scan.export.engine",
    "scan.export.kable_styling",
    "scan.export.title.prefix"
  )
  old_opt <- lapply(setNames(app_options, app_options), getOption)
  on.exit(options(old_opt), add = TRUE)
  
  old_opt <- c(old_opt, options(
    scan.shiny.theme = theme,
    shiny.launch.browser = identical(browser, "external")
  ))
  
  if (inherits(scdf, "scdf")) {
    old_opt <- c(old_opt, options(scan.shinyscan.initial = scdf))
  }
  
  app_dir <- system.file("shiny_scan", package = "scan")
  if (app_dir == "") 
    abort("Cannot find inst/shiny_scan in installed package.")
  
  shiny::runApp(app_dir, quiet = quiet, ...)
  
}