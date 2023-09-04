#' @rdname print.sc
#' @export
#' 
print.sc_pem <- function(x, ...) {
  cat("Percent Exceeding the Median\n\n")
  print(x$PEM, digits = 3, row.names = FALSE)
  cat("\n")
  if (x$decreasing) {
    cat("Assumed decreasing values in the B-phase.\n\n")
    cat("Alternative hypothesis: true probability < 50%\n")
  } else {
    cat("Alternative hypothesis: true probability > 50%\n")
  }
}

