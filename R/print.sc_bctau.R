#' @rdname print.sc
#' @param nice If set TRUE (default) output values are rounded and optimized for
#' publication tables.
#' @export
print.sc_bctau <- function(x, nice = TRUE, digits = "auto", ...) {
  
  cat("Baseline corrected tau\n\n")
  
  if (x$repeated) {
    cat("Method: Siegel repeated median regression\n")
  } else {
    cat("Method: Theil-Sen regression\n")
  }
  
  if (x$continuity) {
    cat("Continuity correction applied\n")
  } else {
    cat("Continuity correction not applied.\n")
  }
  cat("\n")
  if (digits == "auto") {
    x$parameters$p <- round(x$parameters$p, 3)
    x$parameters$z <- sprintf("%.2f", x$parameters$z)
    x$parameters$tau <- sprintf("%.2f", x$parameters$tau)
  } else {
    x$parameters$p <- round(x$parameters$p, digits)
    x$parameters$z <- round(x$parameters$z, digits)
    x$parameters$tau <- round(x$parameters$tau, digits)
  }
  
  if (nice) {
    x$parameters$p <- .nice_p(x$parameters$p)
  }
  
  rownames(x$parameters) <- x$parameters$Model
  print(x$parameters[,-1], ...)
  
  cat("\n")
  if (x$correction)  cat("Baseline correction should be applied.\n\n")
  if (!x$correction) cat("Baseline correction should not be applied.\n\n")
  
}
