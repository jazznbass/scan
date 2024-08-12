#' @rdname print.sc
#' @export
#' 
print.sc_trend <- function(x, digits = 3, ...) {
  x$trend <- round(x$trend, digits)
  cat("Trend for each phase\n\n")
  print(x$trend)
  cat("\n")
  cat("Note. Measurement-times start at", x$first_mt, "for each phase\n")
  .note_vars(x)
}
