#' @describeIn hplm Print results
#' @inheritParams print.sc
#' @param casewise Returns the effect estimations for each case
#' @param smd If TRUE, reports between-case standardized mean differences.
#' @order 2
#' @param x An object returned by [hplm()]
#' @export
print.sc_hplm <- function(x, 
                          digits = 3, 
                          smd = FALSE, 
                          casewise = FALSE,
                          ...) {
  
  out <- .output_hplm(x, casewise = casewise, smd = smd)
  
  cat("Hierarchical Piecewise Linear Regression\n\n")
  cat("Estimation method", x$model$estimation.method,"\n")
  cat("Contrast model: ", out$model, "\n", sep = "")
  cat(x$N, "Cases\n\n")

  cat("AIC = ", out$AIC, ", BIC = ", out$BIC, "\n", sep = "")
  if (!is.null(out$icc)) cat(out$icc, "\n")
  
  cat("\nFixed effects (", out$formula$fixed, ")\n\n", sep = "")
  print(round_numeric(out$fixed, digits))

  cat("\nRandom effects (", out$formula$random ,")\n\n", sep = "")
  print(format_table(out$random, digits = digits, integer = "df"), ...)
  
  if (!is.null(out$correlation)) {
    cat("\nCorrelation:\n")
    print(out$correlation)
  }
  
  if (!is.null(out$bc_smd)) {
    cat("\nBetween-Case Standardized Mean Difference\n\n")
    print(out$bc_smd, digits = digits, row.names = FALSE)
  }
  
  if (!is.null(out$casewise)) {
    cat("\nCasewise estimation of effects\n\n")
    print(out$casewise, row.names = FALSE)
  }
}

.output_hplm <- function(x, casewise = FALSE, smd = FALSE) {
  
  out <- list()
  
  summary_model <- summary(x$hplm)
  
  out$AIC <- summary_model$AIC
  out$BIC <- summary_model$BIC
  
  out$formula$fixed <- deparse(x$model$fixed)
  out$formula$random <- deparse(x$model$random)
  
  out$model <- paste0(
    x$model$interaction.method, " / ", 
    paste0(names(x$contrast), ": ",x$contrast, collapse = ", ")
  )
  
  if (x$model$ICC) {
    out$icc <- sprintf("ICC = %.3f; L = %.1f; p = %.3f", 
                       x$ICC$value, x$ICC$L, x$ICC$p)
  }
  
  # fixed ----
  
  md <- as.data.frame(summary_model$tTable)
  colnames(md) <- c("B", "SE", "df", "t", "p")
  row.names(md) <- rename_predictors(row.names(md), x)
  out$fixed <- md
 
  
  # random -----
  
  sd <- round(as.numeric(VarCorr(x$hplm)[,"StdDev"]), 3)
  md <- data.frame("SD" = sd)
  row.names(md) <- rename_predictors(names(VarCorr(x$hplm)[, 2]), x)
  
  if (x$model$lr.test) {
    if (is.null(x$LR.test[[1]]$L.Ratio)) {
      x$LR.test[[1]]$L.Ratio <- NA
      x$LR.test[[1]]$"p-value" <- NA
      x$LR.test[[1]]$df <- NA
    }
    
    md$L  <- c(unlist(lapply(x$LR.test, function(x) x$L.Ratio[2])), NA)
    md$df <- c(unlist(lapply(x$LR.test, function(x) x$df[2] - x$df[1])), NA)
    md$p  <- c(unlist(lapply(x$LR.test, function(x) x$"p-value"[2])), NA)
  }
  
  out$random <- md
  
  # correlation ----
  
  cov_matrix <- getVarCov(x$hplm)
  var_sd <- sqrt(diag(cov_matrix))
  cor_matrix <- cov_matrix / (var_sd %o% var_sd)
  cor_matrix <- as.data.frame(round(cor_matrix[,], 2))
  row.names(cor_matrix) <- rename_predictors(rownames(cor_matrix), x)
  colnames(cor_matrix) <- rename_predictors(colnames(cor_matrix), x)
  cor_matrix[upper.tri(cor_matrix, diag = TRUE)] <- ""
  cor_matrix <- cor_matrix[-1,-ncol(cor_matrix)]
  
  if (nrow(cor_matrix) > 0) out$correlation <- cor_matrix
  
  # casewise ------
  
  if (casewise) out$casewise <- coef(x, casewise = TRUE)
  
  # smd ----
  
  if (smd) {
    out$bc_smd <- between_smd(x)$bc_smd[, -1, drop = FALSE]
  }
  
  out
  
}