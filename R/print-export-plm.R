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
    cat(
      sprintf(
        paste0("F(%d, %d) = %.2f; p = %0.3f; R\u00b2 = %0.3f; ",
        "Adjusted R\u00b2 = %0.3f\n\n"), 
        x$F.test["df1"], x$F.test["df2"], x$F.test["F"], 
        x$F.test["p"],   x$F.test["R2"],  x$F.test["R2.adj"])
    )	
  }
  
  out <- .prepare_plm_coef(x)
  print(out)
  
  cat("\n")
  if (x$family == "gaussian" && lag_max > 0) {
    cat("Autocorrelations of the residuals\n")
    
    cr <- acf(
      residuals(x$full.model), 
      lag.max = lag_max,plot = FALSE
    )$acf[2:(1 + lag_max)]
    
    cr <- round(cr, 2)
    print(data.frame(lag = 1:lag_max, cr = cr), row.names = FALSE)
    
    if (x$ar == 0) {
      bj <- Box.test(residuals(x$full.model), lag_max, type = "Ljung-Box")
      cat(sprintf(
        "Ljung-Box test: X\u00b2(%d) = %.2f; p = %0.3f", 
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
}

#' @rdname export
#' @param nice If set TRUE (default) output values are rounded and optimized for
#' publication tables.
#' @export
export.sc_plm <- function(object, 
                          caption = NA, 
                          footnote = NA, 
                          filename = NA,
                          kable_styling_options = list(), 
                          kable_options = list(), 
                          nice = TRUE,
                          ...) {
  
  kable_options <- .join_kabel(kable_options)
  kable_styling_options <- .join_kabel_styling(kable_styling_options)
  
  if (is.na(caption)) {
    caption <- paste0(
      "Piecewise-regression model predicting variable '", 
      attr(object, opt("dv")), "'"
    )
  }
  
  out <- .prepare_plm_coef(object)
  
  if (nice) out$p <- .nice_p(out$p)
  
  out <- cbind(Parameter = rownames(out), out, stringsAsFactors = FALSE)
  rownames(out) <- NULL
  
  if (object$family == "poisson" || object$family == "nbinomial") {
    Chi <- object$full$null.deviance - object$full$deviance
    DF <- object$full$df.null - object$full$df.residual
    F_test <- sprintf(
      "X\u00b2(%d) = %.2f; p %s; AIC = %.0f", 
      DF, 
      Chi, 
      .nice_p(1 - pchisq(Chi, df = DF), TRUE), 
      object$full$aic
    )
  }
  if (object$family == "gaussian") {
    F_test <- sprintf(
      "F(%d, %d) = %.2f; p %s; R\u00b2 = %0.3f; Adjusted R\u00b2 = %0.3f", 
      object$F.test["df1"], 
      object$F.test["df2"], 
      object$F.test["F"], 
      .nice_p(object$F.test["p"], TRUE), 
      object$F.test["R2"], object$F.test["R2.adj"]
    )
  }
  
  if (is.na(footnote)) footnote <- F_test
  
  if (getOption("scan.export.engine") == "gt") {
    if (object$family == "gaussian") {
      spanner <- list("CI(95%)" = 3:4)
    }
    
    if (object$family %in% c("poisson", "nbinomial")) {
      spanner <- list("CI(95%)" = 3:4, " CI(95%) " = 9:10, "  CI(95%)  " = 12:13)
    }
  }

  table <- .create_table(
    out, 
    kable_options, 
    kable_styling_options, 
    caption = caption,
    footnote = footnote,
    spanner = spanner
  )
  
  if (getOption("scan.export.engine") == "kable") {
    if (object$family == "gaussian") {
      table <- add_header_above(table, c(" " = 2, "CI(95%)" = 2, " " = 4))
    }
    
    if (object$family %in% c("poisson", "nbinomial")) {
      table <- add_header_above(
        table, 
        c(" " = 2, "CI(95%)" = 2, " " = 4, "CI(95%)" = 2," " = 1, "CI(95%)" = 2 )
      )
    }
  }

  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) .save_export(table, filename)
  table
  
}

# helper functions -----

.prepare_plm_coef <- function(x) {
  
  if (x$ar == 0) out <- summary(x$full.model)$coefficients
  if (x$ar  > 0) out <- summary(x$full.model)$tTable

  inter_incl <- if (attr(x$full.model$terms, "intercept")) TRUE else FALSE
  
  ci <- suppressMessages(confint(x$full))
  param_filter <- apply(ci, 1, function(x) if(!all(is.na(x))) TRUE else FALSE)
  ci <- ci[param_filter, ]
  
  if (nrow(out) == 1) {
    out <- cbind(
      out[, 1, drop = FALSE], 
      t(ci), 
      out[, 2:4, drop = FALSE]
    )
  } else out <- cbind(
    out[,1], 
    ci, 
    out[, 2:4]
  )
  
  out <- round(out, 3)
  out <- as.data.frame(out)
  if (!is.null(x$r.squares)) {
    x$r.squares <- x$r.squares[c(param_filter[-1])]
    out$R2 <- c(rep("", inter_incl), format(round(x$r.squares, 3)))
  }
  
  row.names(out) <- .plm.row.names(row.names(out), x)
  
  colnames(out) <- if (is.null(x$r.squares)) {
    c("B", "2.5%", "97.5%", "SE", "t", "p")
  } else {
    c("B", "2.5%", "97.5%", "SE", "t", "p", "delta R\u00b2")
  }  
  
  if (x$family == "poisson" || x$family == "binomial") {
    OR <- exp(out[, 1:3])
    Q <- (OR - 1) / (OR + 1)
    out <- cbind(out[, -7], round(OR, 3), round(Q, 2))
    colnames(out) <- c(
      "B", "2.5%", "97.5%", "SE", "t", "p", "Odds Ratio", 
      " 2.5% ", " 97.5% ", "Yule's Q", "  2.5%  ", "  97.5%  "
    )		
  }
  out
}
