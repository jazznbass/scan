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
  
  cat("Kendall's tau", x$tau_method, "applied.\n")
  if (x$continuity) {
    cat("Continuity correction applied\n")
  } else {
    cat("Continuity correction not applied.\n")
  }
  cat("\n")
  
  for(i in seq_along(x$corrected_tau)) {
    if (digits == "auto") {
      x$corrected_tau[[i]]$p <- round(x$corrected_tau[[i]]$p, 3)
      x$corrected_tau[[i]]$z <- sprintf("%.2f", x$corrected_tau[[i]]$z)
      x$corrected_tau[[i]]$tau <- sprintf("%.2f", x$corrected_tau[[i]]$tau)
    } else {
      x$corrected_tau[[i]]$p <- round(x$corrected_tau[[i]]$p, digits)
      x$corrected_tau[[i]]$z <- round(x$corrected_tau[[i]]$z, digits)
      x$corrected_tau[[i]]$tau <- round(x$corrected_tau[[i]]$tau, digits)
    }
    
    if (nice) {
      x$corrected_tau[[i]]$p <- .nice_p(x$corrected_tau[[i]]$p)
    }
    
    rownames(x$corrected_tau[[i]]) <- x$corrected_tau[[i]]$Model
    cat(names(x$corrected_tau)[i], ":\n")
    print(x$corrected_tau[[i]][,-1], ...)
    cat("\n")
 
    if (x$correction[[i]]) cat("Baseline correction should be applied.\n\n")
    if (!x$correction[[i]]) cat("Baseline correction should not be applied.\n\n")
  }
  

  
  cat("\n")

  
}
