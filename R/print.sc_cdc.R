#' @rdname print.sc
#' @export
print.sc_cdc <- function(x, nice = TRUE, ...) {
  
  cat("Conservative Dual Criterion\n\n")
  cat("N cases = ", x$N, "\n\n")
  
  if (nice) x$cdc_p <- .nice_p(x$cdc_p)
  out <- data.frame(
    Case = x$case_names,
    "nB improve" = x$cdc_be,
    "nB" = x$cdc_b,
    "binom p" = x$cdc_p,
    "CDC Evaluation" = x$cdc,
    check.names = FALSE
  )
  print(out, row.names = FALSE)
  cat("\n")
  if (x$decreasing) {
    cat("Assuming an expected decrease in phase B.\n")
    cat("Alternative hypothesis (Binomial test): true probability < 50%\n")
  } else {
    cat("Assuming an expected increase in phase B.\n")
    cat("Alternative hypothesis (Binomial test): true probability > 50%\n")
  }
  if (x$N > 1) {
    cat("Overall evaluation of all MBD instances:  ",x$cdc_all,"\n")
  }
  
}

