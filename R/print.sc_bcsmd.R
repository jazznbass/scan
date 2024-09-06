#' @describeIn between_smd Print results
#' @inheritParams print.sc
#' @order 2
#' @param x An object returned by `baseline_smd()`.
#' @export
print.sc_bcsmd <- function(x, digits = 2, ...) {
  cat("Between-Case Standardized Mean Difference\n\n")

  
  print(round_numeric(x$bc_smd, digits), row.names = FALSE)
}

