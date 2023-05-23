#' @rdname print.sc
#' @param lag_max Maximum lag to be reported for autocorrelation of residuals.
#'   Default is `3`. Set `FALSE` for no report of autocorrelations.
#' @export
#' 
print.sc_plm <- function(x, lag_max = 3, ...) {
  cat("Piecewise Regression Analysis\n\n")
  cat(
    "Contrast model: ", 
    x$model, " / ", 
    paste0("level = ", x$contrast$level, ", slope = " ,x$contrast$slope),
    "\n\n", sep = ""
  )
  
  cat("Fitted a", x$family, "distribution.\n")		
  
  if (x$ar > 0)
    cat("Correlated residuals up to autoregressions of lag",
        x$ar, "are modelled\n\n")
  
  if (x$family == "poisson" || x$family == "binomial") {
    Chi <- x$full$null.deviance - x$full$deviance
    DF <- x$full$df.null - x$full$df.residual
    cat(sprintf(
      "X\u00b2(%d) = %.2f; p = %0.3f; AIC = %.0f\n\n", 
      DF, Chi, 1 - pchisq(Chi, df = DF), x$full$aic)
    )	
  } else {
    cat(sprintf(
      "F(%d, %d) = %.2f; p = %0.3f; R\u00b2 = %0.3f; Adjusted R\u00b2 = %0.3f\n\n", 
      x$F.test["df1"], x$F.test["df2"], x$F.test["F"], 
      x$F.test["p"],   x$F.test["R2"],  x$F.test["R2.adj"])
    )	
  }
  
  if (x$ar == 0) res <- summary(x$full.model)$coefficients
  if (x$ar  > 0) res <- summary(x$full.model)$tTable
   
  if (attr(x$full.model$terms, "intercept")) {
    intercept_included <- TRUE 
  } else{ 
    intercept_included <- FALSE
  }
  
  ci <- suppressMessages(confint(x$full))
  parameter_filter <- apply(ci, 1, function(x) if(!all(is.na(x))) TRUE else FALSE)
  ci <- ci[parameter_filter, ]
  
  if (nrow(res) == 1) {
    res <- cbind(
      res[, 1, drop = FALSE], 
      t(ci), 
      res[, 2:4, drop = FALSE]
    )
  } else res <- cbind(
    res[,1], 
    ci, 
    res[, 2:4]
  )
  
  res <- round(res, 3)
  res <- as.data.frame(res)
  if (!is.null(x$r.squares)) {
    x$r.squares <- x$r.squares[c(parameter_filter[-1])]
    res$R2 <- c(rep("", intercept_included), format(round(x$r.squares, 4)))
  }
  row.names(res) <- .plm.row.names(row.names(res), x)
  
  if (!is.null(x$r.squares))
    colnames(res) <- c("B", "2.5%", "97.5%", "SE", "t", "p", "delta R\u00b2")		
  if (is.null(x$r.squares))
    colnames(res) <- c("B", "2.5%", "97.5%", "SE", "t", "p")		
  
  if (x$family == "poisson" || x$family == "binomial") {
    OR <- exp(res[, 1:3])
    Q <- (OR - 1) / (OR + 1)
    res <- cbind(res[, -7], round(OR, 3), round(Q, 2))
    colnames(res) <- c(
      "B", "2.5%", "97.5%", "SE", "t", "p", "Odds Ratio", 
      "2.5%", "97.5%", "Yule's Q", "2.5%", "97.5%"
    )		
  }
  print(res)
  cat("\n")
  if (x$family == "gaussian" && lag_max > 0) {
    cat("Autocorrelations of the residuals\n")
    
    cr <- acf(residuals(x$full.model), lag.max = lag_max,plot = FALSE)$acf[2:(1 + lag_max)]
    cr <- round(cr, 2)
    print(data.frame(lag = 1:lag_max, cr = cr), row.names = FALSE)
    
    if (x$ar == 0) {
      bj <- Box.test(residuals(x$full.model), lag_max, type = "Ljung-Box")
      cat(sprintf(
        "Ljung-Box test: X-Squared(%d) = %.2f; p = %0.3f", 
        bj$parameter, bj$statistic, bj$p.value
      ), "\n\n"
      )	   
    }

  }
  cat("Formula: ")
  if (x$family == "binomial" && !x$dvar_percentage) {
    x$formula[2] <- str2expression(paste0(x$formula[2], "/", x$var_trials))
  }
  print(x$formula, showEnv = FALSE)
  if (x$family == "binomial") {
    cat("weights = ", x$var_trials)
  }
  cat("\n")
  #.note_vars(x)
}

