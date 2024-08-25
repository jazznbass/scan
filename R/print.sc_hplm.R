#' @describeIn hplm Print results
#' @param casewise Returns the effect estimations for each case
#' @param smd If TRUE, reports between-case standardized mean differences.
#' @order 2
#' @param x An object returned by [hplm()]
#' @export
print.sc_hplm <- function(x, ..., smd = FALSE, casewise = FALSE) {
  cat("Hierarchical Piecewise Linear Regression\n\n")
  cat("Estimation method", x$model$estimation.method,"\n")
  cat("Contrast model: ", 
      x$model$interaction.method, " / ", 
      paste0(names(x$contrast), ": ",x$contrast, collapse = ", "), 
      "\n", sep = "")

  cat(x$N, "Cases\n\n")
  
  out <- list()
  
  if (x$model$ICC) {
    out$ICC <- sprintf("ICC = %.3f; L = %.1f; p = %.3f\n\n", 
                       x$ICC$value, x$ICC$L, x$ICC$p)
    cat(out$ICC)
  }
  
  md <- as.data.frame(summary(x$hplm)$tTable)
  
  colnames(md) <- c("B", "SE", "df", "t", "p")
  
  row.names(md) <- .plm.row.names(row.names(md), x)
  
  md$B  <- round(md$B,  3)
  md$SE <- round(md$SE, 3)
  md$t  <- round(md$t,  3)
  md$p  <- round(md$p,  3)
  
  out$ttable <- md
  
  cat("Fixed effects (",deparse(x$model$fixed),")\n\n", sep = "")
  print(md)
  
  cat("\nRandom effects (",deparse(x$model$random),")\n\n", sep = "")
  SD <- round(as.numeric(VarCorr(x$hplm)[,"StdDev"]), 3)
  md <- data.frame("EstimateSD" = SD)
  rownames(md) <- names(VarCorr(x$hplm)[, 2])
  
  row.names(md) <- .plm.row.names(row.names(md), x)
  
  if (x$model$lr.test) {
    if (is.null(x$LR.test[[1]]$L.Ratio)) {
      x$LR.test[[1]]$L.Ratio <- NA
      x$LR.test[[1]]$"p-value" <- NA
      x$LR.test[[1]]$df <- NA
    }
    
    md$L  <- c(round(unlist(lapply(x$LR.test, function(x) x$L.Ratio[2])), 2), NA)
    md$df <- c(unlist(lapply(x$LR.test,       function(x) x$df[2] - x$df[1])), NA)
    md$p  <- c(round(unlist(lapply(x$LR.test, function(x) x$"p-value"[2])), 3), NA)
  }
  
  print(md, na.print = "-", ...)
  
  if (smd) {
    cat("\nBetween-Case Standardized Mean Difference\n\n")
    print(round_numeric(between_smd(x)$bc_smd, 2), row.names = FALSE)
    
  }
  
  if (casewise) {
    cat("\nCasewise estimation of effects\n\n")
    print(coef(x, casewise = TRUE), row.names = FALSE)
  }
}

