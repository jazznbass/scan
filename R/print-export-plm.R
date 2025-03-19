#' @rdname print.sc
#' @param lag_max Maximum lag to be reported for autocorrelation of residuals.
#'   Default is `3`. Set `FALSE` for no report of autocorrelations.
#' @param ci Print confidence intervals. Either FALSE, TRUE or a number 
#' between 0 and 1 (0.90 for a 90% intervals).
#' @param q Logical. If set `TRUE`, Yule's Q is reported.
#' @export
#' 
print.sc_plm <- function(x, lag_max = 3, ci = 0.95, q = FALSE, ...) {
  cat("Piecewise Regression Analysis\n\n")
  cat(
    "Contrast model: ", 
    x$model, " / ", 
    paste0("level = ", x$contrast$level, ", slope = " ,x$contrast$slope),
    "\n\n", sep = ""
  )
  
  cat("Fitted a", x$family, "distribution.\n")		
  
  results <- .output_plm(x, ci = ci, q = q, lag_max = lag_max, format = "print")
  
  if (x$ar > 0) {
    cat(
      "Correlated residuals up to autoregressions of lag",
      x$ar, "are modelled\n\n"
    )
  }
  
  cat(results$fit, "; AIC = ", results$aic, "\n\n", sep = "")
  
  print(results$table)
  cat("\n")
  
  if (!is.null(results$autocorrelation)) {
    cat("Autocorrelations of the residuals\n")
    print(results$autocorrelation, row.names = FALSE)
    cat(results$ljung, "\n\n")
  }
  
  cat("Formula:", results$formula, "\n")
}

#' @rdname export
#' @param nice If set TRUE (default) output values are rounded and optimized for
#' publication tables.
#' @param ci Print confidence intervals. Either FALSE, TRUE or a number 
#' between 0 and 1 (0.90 for a 90% intervals).
#' @param q Logical. If set `TRUE`, Yule's Q is reported.
#' @export
export.sc_plm <- function(object, 
                          caption = NA, 
                          footnote = NA, 
                          filename = NA,
                          kable_styling_options = list(), 
                          kable_options = list(), 
                          nice = TRUE,
                          ci = 0.95,
                          q = FALSE,
                          ...) {
  
  kable_options <- .join_kabel(kable_options)
  kable_styling_options <- .join_kabel_styling(kable_styling_options)
  
  if (is.na(caption)) {
    caption <- paste0(
      "Piecewise-regression model predicting '", attr(object, opt("dv")), "'"
    )
  }
  
  results <- .output_plm(object, ci = ci, q = q, format = "export")
  
  out <- results$table
  out <- rownames_to_first_column(out, "Parameter")
  
  if (nice) out$p <- .nice_p(out$p)
  
  if (is.na(footnote)) footnote <- c(
    paste0(results$fit), 
    paste0("AIC = ", round(results$aic)),
    "LL = lower limit",
    "UL = upper limit"
  )
  
  if (getOption("scan.export.engine") == "gt") {
    spanner <- list("CI" = 3:4)
    if (object$family %in% c("poisson", "nbinomial")) {
      spanner[[" CI "]]  <- 9:10
      if (q) spanner[["  CI  "]]  <- 12:13
    }
    names(spanner) <- gsub(
      "CI", paste0("CI(", ci * 100, "%)"), x = names(spanner)
    )
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
    spanner <- c(" " = 2, "CI" = 2, " " = 4)
  
    if (object$family %in% c("poisson", "nbinomial")) {
      spanner <- c(spanner, "CI" = 2)
      if (q) spanner <- c(spanner, " " = 1, "CI" = 2)
    }
    
    names(spanner) <- gsub(
      "CI", paste0("CI(", ci * 100, "%)"), x = names(spanner)
    )
    
    table <- add_header_above(table, spanner)
    
  }

  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) .save_export(table, filename)
  table
  
}

.output_plm <- function(x, ci, q, lag_max = 0, format) {
  
  out <- list()
  
  out$aic <- x$full.model$aic
  if (x$family == "poisson" || x$family == "binomial") {
    chi <- x$full$null.deviance - x$full$deviance
    df <- x$full$df.null - x$full$df.residual
    out$fit <- sprintf(
      "X\u00b2(%d) = %.2f; p = %0.3f", 
      df, chi, 1 - pchisq(chi, df = df)
    )
  } else {
    out$fit <- if (x$F.test["df1"] == 0) {
      "Null model" 
    } else {
      sprintf(
        "F(%d, %d) = %.2f; p = %0.3f; R\u00b2 = %0.3f; Adjusted R\u00b2 = %0.3f", 
        x$F.test["df1"], x$F.test["df2"], x$F.test["F"], 
        x$F.test["p"],   x$F.test["R2"],  x$F.test["R2.adj"]
      )
    }
  }
  
  ## coef table ----
  
  if (x$ar == 0) out$table <- summary(x$full.model)$coefficients
  if (x$ar  > 0) out$table <- summary(x$full.model)$tTable
  
  inter_incl <- if (attr(x$full.model$terms, "intercept")) TRUE else FALSE
  
  out$table <- round(out$table, 3)
  out$table <- as.data.frame(out$table)
  names(out$table)[1:4] <- c("B", "SE", "t", "p")
  
  if (identical(ci, FALSE)) {
    
    if (!is.null(x$r.squares)) {
      out$table[["delta R\u00b2"]] <- c(
        rep("", inter_incl), 
        format(round(x$r.squares, 3))
      )
    }
    
    if (x$family == "poisson" || x$family == "binomial") {
      OR <- exp(out$table[, "B"])
      out$table <- cbind(out$table[, c("B", "SE", "t", "p")],  "OR" = round(OR$table, 3))
      if (q) {
        Q <- (OR - 1) / (OR + 1)
        out$table <- cbind(out$table, "Q" = round(Q, 2))
      }
    }
  }
  
  ## ci ----
  if (!identical(ci, FALSE)) {
    if (isTRUE(ci)) ci <- 0.95
    
    #str_ci <- paste0(round(c((1 - ci) / 2, ci + ((1 - ci) / 2)) * 100, 2), "%")
    
    if (format == "print") str_ci <- paste0(c("LL", "UL"), "-CI", ci * 100, "%")
    if (format == "export") str_ci <- c("LL", "UL")
    
    ci <- suppressMessages(confint(x$full, level = ci))
    
    if (length(ci) == 2) ci <- matrix(ci, ncol = 2)
    param_filter <- apply(
      ci, 1, function(x) if(!all(is.na(x))) TRUE else FALSE
    )
    ci <- round(ci[param_filter, ], 3)
    if (nrow(out$table) == 1) ci <- t(ci)
    out$table <- cbind(
      out$table[, 1, drop = FALSE], 
      ci, 
      out$table[, 2:4, drop = FALSE]
    )
    
    names(out$table) <- c("B", str_ci, "SE", "t", "p")
    
    if (!is.null(x$r.squares)) {
      x$r.squares <- x$r.squares[c(param_filter[-1])]
      out$table[["delta R\u00b2"]] <- c(rep("", inter_incl), format(round(x$r.squares, 3)))
    }
    
    if (x$family == "poisson" || x$family == "binomial") {
      OR <- exp(out$table[, 1:3])
      out$table <- cbind(out$table[, 1:6], round(OR, 3))
      names(out$table)[7:9] <- c("OR", paste0(" ", str_ci)) 
      
      if(q) {
        Q <- (OR - 1) / (OR + 1)
        out$table <- cbind(out$table, round(Q, 2))
        names(out$table)[10:12] <- c("Q", paste0("  ", str_ci))  
      }
      
    }
  } 
  rownames(out$table) <- rename_predictors(rownames(out$table), x)
  
  ## autocorrelation
  
  if (x$family == "gaussian" && lag_max > 0) {
    
    cr <- acf(
      residuals(x$full.model), 
      lag.max = lag_max,plot = FALSE
    )$acf[2:(1 + lag_max)]
    
    cr <- round(cr, 2)
    out$autocorrelation <- data.frame(lag = 1:lag_max, cr = cr)
    
    #if (x$ar == 0) {
      bj <- Box.test(residuals(x$full.model), lag_max, type = "Ljung-Box")
      out$ljung <- sprintf(
        "Ljung-Box test: X\u00b2(%d) = %.2f; p = %0.3f", 
        bj$parameter, bj$statistic, bj$p.value
      )
    #}
    
  }
  
  ## formula ----
  
  if (x$family == "binomial" && !x$dvar_percentage) {
    x$formula[2] <- str2expression(paste0(x$formula[2], "/", x$var_trials))
  }
  out$formula <- deparse(x$formula)
  if (x$family == "binomial") {
    out$formula <- paste0(out$formula, "\n", "weights = ", x$var_trials)
  }
  out
}

