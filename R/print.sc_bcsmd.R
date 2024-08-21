#' @describeIn between_ Print results
#' @order 2
#' @param x An object returned by [baseline_smd()]
#' @export
print.sc_bcsmd <- function(x, ...) {
  cat("Between-Case Standardized Mean Difference\n\n")

  print(x$bc_smd, row.names = FALSE)
}

