#' @describeIn between_smd Print results
#' @inheritParams print.sc
#' @order 2
#' @param x An object returned by `baseline_smd()`.
#' @export
print.sc_bcsmd <- function(x, digits = 2, ...) {
  cat("Between-Case Standardized Mean Difference\n\n")

  cat("Model:", x$model, "\n\n")
  
  bc_smd <- round_numeric(x$bc_smd, digits)
  #names(bc_smd) <- rename_predictors(names(bc_smd), x)
  print(bc_smd, row.names = FALSE)
}

