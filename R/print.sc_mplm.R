#' @rdname print.sc
#' @param std If TRUE, a table with standardized estimates is included.
#' @export
print.sc_mplm <- function(x, digits = "auto", std = FALSE, ...) {
  
  if (digits == "auto") digits <- 3
  
  cat("Multivariate piecewise linear model\n\n")
  cat(
    "Dummy model: ", x$model, " ", 
    paste0("level = ", x$contrast_level, ", slope = " ,x$contrast_slope),
    "\n\n", 
    sep = ""
  )
   
  coef <- x$full.model$coefficients
  if (inherits(coef, "numeric"))  {
    coef <- as.data.frame(coef)
    names(coef) <- attr(x, opt("dv"))
  }
  
  rownames(coef) <- gsub("(Intercept)", "Intercept", rownames(coef))
  rownames(coef) <- gsub("mt", "Trend", rownames(coef))
  rownames(coef) <- gsub("phase", "Level Phase ", rownames(coef))
  rownames(coef) <- gsub("inter", "Slope Phase ", rownames(coef))
  
  cat("Coefficients: \n")
  print(coef, digits = digits, ...)
  
  if (isTRUE(std)) { 
    coef_std <- x$full.model$coef_std
    rownames(coef_std) <- gsub("(Intercept)", "Intercept", rownames(coef_std))
    rownames(coef_std) <- gsub("mt", "Trend", rownames(coef_std))
    rownames(coef_std) <- gsub("phase", "Level Phase ", rownames(coef_std))
    rownames(coef_std) <- gsub("inter", "Slope Phase ", rownames(coef_std))
    
    cat("\nStandardized coefficients: \n")
    print(coef_std, digits = digits, ...)
  }
  
  cat("\n")
  cat("Formula: ")
  print(x$formula, showEnv = FALSE)
  res <- car::Anova(x$full.model, type = 3)
  if (!is.null(res$terms)) {
    res$terms <- gsub("(Intercept)", "Intercept", res$terms)
    res$terms <- gsub("mt", "Trend", res$terms)
    res$terms <- gsub("phase", "Level Phase ", res$terms)
    res$terms <- gsub("inter", "Slope Phase ", res$terms)
  } else {
    rn <- attr(res, "row.names")
    rn <- gsub("(Intercept)", "Intercept", rn)
    rn <- gsub("mt", "Trend", rn)
    rn <- gsub("phase", "Level Phase ", rn)
    rn <- gsub("inter", "Slope Phase ", rn)
    attr(res, "row.names") <- rn
  }
    
  print(res, digits = digits, ...)
  .note_vars(x)
  
}

