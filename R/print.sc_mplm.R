#' @describeIn mplm Print results
#' @order 2
#' @inheritParams print.sc
#' @param x Object returned from [mplm()]. 
#' @param digits The minimum number of significant digits to be use. 
#' If set to "auto" (default), values are predefined.
#' @param std If TRUE, a table with standardized estimates is included.
#' @export
print.sc_mplm <- function(x, digits = "auto", std = FALSE, ...) {
  
  if (digits == "auto") digits <- 3
  
  cat("Multivariate piecewise linear model\n\n")
  cat(
    "Dummy model: ", x$model, " ", 
    paste0("level = ", x$contrast$level, ", slope = " ,x$contrast$slope),
    "\n\n", 
    sep = ""
  )
   
  coef <- x$full.model$coefficients
  if (inherits(coef, "numeric"))  {
    coef <- as.data.frame(coef)
    names(coef) <- attr(x, opt("dv"))
  }
  
  row.names(coef) <- rename_predictors(row.names(coef), x)

  cat("Coefficients: \n")
  print(coef, digits = digits, ...)
  
  if (isTRUE(std)) { 
    coef_std <- x$full.model$coef_std
    row.names(coef_std) <- rename_predictors(row.names(coef_std), x)

    cat("\nStandardized coefficients: \n")
    print(coef_std, digits = digits, ...)
  }
  
  cat("\n")
  cat("Formula: ")
  print(x$formula, showEnv = FALSE)
  res <- car::Anova(x$full.model, type = 3)
  
  if (!is.null(res$terms)) {
    res$terms <- rename_predictors(res$terms, x)
  } else {
    row.names(res) <- rename_predictors(row.names(res), x)
  }
    
  print(res, digits = digits, ...)
  .note_vars(x)
}

