
#' @export
shinyscan <- function() {
  runApp(list(ui = ui, server = server), launch.browser = TRUE)
}
