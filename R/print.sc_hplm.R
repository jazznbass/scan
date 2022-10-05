#' @rdname print.sc
#' @export
print.sc_hplm <- function(x, ...) {
  cat("Hierarchical Piecewise Linear Regression\n\n")
  cat("Estimation method", x$model$estimation.method,"\n")
  cat("Contrast model: ", 
      x$model$interaction.method, " / ", 
      paste0(names(x$model$contrast.method), ": ",x$model$contrast.method, collapse = ", "), 
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
}

