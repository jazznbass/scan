#' Print method for scan objects
#'
#' @param x Object 
#' @param digits The minimum number of significant digits to be use. 
#' If set to "auto" (default), values are predefnied.
#' @param ... Further parameters passed to the print function
#' @export

print.sc <- function(x, digits = "auto", ...) {
  
  value <- class(x)[2]
  note <- FALSE
  
  args <- list(...)
  if (is.null(args)) args <- list()

# baseline corrected tau --------------------------------------------------
  if (value == "base_corr_tau") {
    
    if (digits == "auto") digits <- 2
    
    cat("Baseline corrected tau\n\n")
    cat("Auto correlation in baseline:\n")
    cat("tau =", round(x$auto_tau$tau, 2))
    cat("; p =", round(x$auto_tau$p, 3), "\n\n")
    
    cat("Baseline corrected tau:\n")
    cat("tau =", round(x$tau, digits))
    cat("; p =", round(x$p, 3),"\n\n")
    if (x$correction)  cat("Baseline correction applied.\n\n")
    if (!x$correction) cat("Baseline correction not applied.\n\n")
    
  }


# overlap -----------------------------------------------------------------

  if (value == "overlap") {
    class(x) <- "sc_overlap"
    print(x, ...)
  }

# TAU-U -------------------------------------------------------------------

  if (value == "TAU-U") {	
    
    if (digits == "auto") digits <- 3
    
    cat("Tau-U\n")
    cat("Method:", x$method, "\n")
    cat("Applied Kendall's Tau-", x$tau_method, "\n\n", sep = "")
    
    out <- x$table
    
    if (length(out) > 1) {
      cat("Overall Tau-U: \n")
      print(x$Overall_tau_u, row.names = FALSE, digits = digits)
      cat("\n")
    }
    
    complete <- FALSE
    if (any(names(args) == "complete")) complete <- args$complete
    if (!complete) {
      select_vars <- c("Tau", "SE_Tau", "Z", "p")
      select_rows <- match(
        c(
          "A vs. B", 
          "A vs. B - Trend A",
          "A vs. B + Trend B", 
          "A vs. B + Trend B - Trend A"
        ), row.names(x$table[[1]])
      )
        
      out <- lapply(x$table, function(x) round(x[select_rows, select_vars], 3))
    }
    
    for(i in seq_along(out)) {
      cat("Case:", names(out)[i], "\n")
      print(out[[i]], digits = digits)
      cat("\n")
    }
  }
  
# power -------------------------------------------------------------------

  if (value == "power") {	
    cat("Test-Power in percent:\n")
    ma <- matrix(
      unlist(x[1:16]) * 100, byrow = FALSE, ncol = 2, 
      dimnames = list(
        c("tauU: A vs. B - Trend A", 
          paste0("Rand-Test: ",x$rand.test.stat[1]),  
          "PLM.Norm: Level", 
          "PLM.Norm: Slope", 
          "PLM.Poisson: Level", 
          "PLM.Poisson: Slope", 
          "HPLM: Level", 
          "HPLM: Slope"), 
        c("Power", "Alpha-error")
        ))
    print(ma)
  }

# PET ---------------------------------------------------------------------
  
  if (value == "PET") {	
    cat("Percent Exceeding the Trend\n\n")
    cat("N cases = ", x$N, "\n")
    cat("\n")
    ma <- cbind(x$PET, x$p, x$PET.ci)
    colnames(ma) <- c("PET", "binom.p", "PET CI")
    rownames(ma) <- x$case.names
    print(round(ma, 3))
    cat("\n")
    
    if (x$decreasing) {
      cat("Assumed decreasing values in the B-phase.\n\n")
      cat("Binom.test: alternative hypothesis: true probability < 50%\n")
      cat(sprintf("PET CI: Percent of values less than lower %d%% confidence threshold (smaller %.3f*se below predicted value)\n", x$ci,x$se.factor))
    } else {
      cat("Binom.test: alternative hypothesis: true probability > 50%\n")
      cat(sprintf("PET CI: Percent of values greater than upper %d%% confidence threshold (greater %.3f*se above predicted value)\n", x$ci,x$se.factor))
    }
    
  }	

# NAP ---------------------------------------------------------------------

  if (value == "NAP") {
    
    if (digits == "auto") digits <- 2
    
    cat("Nonoverlap of All Pairs\n\n")
    print(x$nap, row.names = FALSE, digits = digits)
  }

# PEM ---------------------------------------------------------------------

  if (value == "PEM") {
    cat("Percent Exceeding the Median\n\n")
    ma <- cbind(PEM = x$PEM, x$test)
    print(round(ma,3))
    cat("\n")
    if (x$decreasing) {
      cat("Assumed decreasing values in the B-phase.\n\n")
      cat("Alternative hypothesis: true probability < 50%\n")
    } else {
      cat("Alternative hypothesis: true probability > 50%\n")
    }
  }

# PND ---------------------------------------------------------------------

  if (value == "PND") {
    cat("Percent Non-Overlapping Data\n\n")
    out <- data.frame(
      Case = x$case.names, 
      PND = paste0(round(x$PND, 2),"%"), 
      "Total" = x$n.B, 
      "Exceeds" = round(x$PND / 100 * x$n.B)
    )
    print(out, row.names = FALSE)
    cat("\nMean  :", round(mean(x$PND, na.rm = TRUE), 2),"%\n")
  }	

# trend -------------------------------------------------------------------

  if (value == "trend") {
    x$trend <- round(x$trend, 3)
    cat("Trend for each phase\n\n")
    #cat("N cases = ", x$N,"\n")
    #cat("\n")
    print(x$trend)
    cat("\n")
    cat("Note. Measurement-times of phase B start at", 1 + x$offset, "\n")
  }

# rci -------------------------------------------------------------------

  if (value == "rci") {
    #cat("!!! Caution! This function is under development and not yet ready for use!!!\n\n")
    cat("Reliable Change Index\n\n")
    cat("Mean Difference = ", x$descriptives[2, 2] - x$descriptives[1, 2], "\n")
    cat("Standardized Difference = ", x$stand.dif, "\n")
    cat("\n")
    cat("Descriptives:\n")
    print(x$descriptives)
    cat("\n")
    cat("Reliability = ", x$reliability, "\n")
    cat("\n")
    cat(x$conf.percent * 100, "% Confidence Intervals:\n")
    print(x$conf)
    cat("\n")
    cat("Reliable Change Indices:\n")
    print(x$RCI)
    cat("\n")
  }
  
# rand --------------------------------------------------------------------

  if (value == "rand") {
    cat("Randomization Test\n\n")
    if (x$N > 1) cat("Test for", x$N, "cases.\n\n")

    cat(.stringPhasesSC(x$phases.A, x$phases.B), "\n")
    
    cat("Statistic: ",x$statistic,"\n\n")
    if (is.na(x$startpoints[1])) {
      cat("Minimal length of each phase: ", x$limit, "\n")
    } else {
      cat("Possible starting points of phase B: ", x$startpoints, "\n")
    }
    cat("Observed statistic = ", x$observed.statistic, "\n")
    cat("\n")
    if (x$auto.corrected.number) {
      cat("Warning! The assigned number of random permutations exceeds the number of possible permutations.\nAnalysis is restricted to all possible permutations.\n")
    }
    if (x$complete) {
      cat("\nDistribution based on all", x$possible.combinations,"possible combinations.\n")
    } else 
      cat("\nDistribution based on a random sample of all", x$possible.combinations, "possible combinations.\n")
    
    cat("n   = ", x$number,"\n")
    cat("M   = ", mean(x$distribution),"\n")
    cat("SD  = ", sd(x$distribution),"\n")
    cat("Min = ", min(x$distribution),"\n")
    cat("Max = ", max(x$distribution),"\n")
    cat("\n")
    if (x$p.value == 0)
      cat("p   < ", format(1/x$number, scientific = FALSE), "\n")
    else
      cat("p   = ", x$p.value, "\n")
    if (x$number > 3 & x$number < 5001) {
      sh <- shapiro.test(x$distribution)
      cat(sprintf("\nShapiro-Wilk Normality Test: W = %0.3f; p = %0.3f",sh[[1]], sh$p.value))
      if (sh$p.value > .05)
        cat("  (Hypothesis of Normality maintained)\n")
      else
        cat("  (Hypothesis of Normality rejected)\n")
    } else cat("\nSample size must be between 3 and 5000 to perform a Shapiro-Wilk Test.\n")
    cat(sprintf("z = %0.4f, p = %0.4f (single sided)\n", x$Z, x$p.Z.single))
  }
  
# plm ---------------------------------------------------------------------

  if (value == "pr"){
    cat("Piecewise Regression Analysis\n\n")
    cat("Dummy model: ", x$model,"\n\n")
    cat("Fitted a", x$family, "distribution.\n")		
    
    if (x$ar > 0)
      cat("Correlated residuals up to autoregressions of lag",
          x$ar, "are modelled\n\n")
    
    if (x$family == "poisson" || x$family == "nbinomial") {
      Chi <- x$full$null.deviance - x$full$deviance
      DF <- x$full$df.null - x$full$df.residual
      cat(sprintf(
        "\u0347\u00b2(%d) = %.2f; p = %0.3f; AIC = %.0f\n\n", 
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
    if (nrow(res) == 1) {
      res <- cbind(
        res[, 1, drop = FALSE], 
        t(suppressMessages(confint(x$full))), 
        res[, 2:4, drop = FALSE]
      )
    } else res <- cbind(
        res[,1], 
        suppressMessages(confint(x$full)), 
        res[, 2:4]
      )
    
    res <- round(res,3)
    res <- as.data.frame(res)
    if (!is.null(x$r.squares)) 
      res$R2 <- c("", format(round(x$r.squares, 4)))
    
    row.names(res) <- .plm.row.names(row.names(res), x)
   
    if (!is.null(x$r.squares))
      colnames(res) <- c("B","2.5%","97.5%","SE", "t","p", "delta R\u00b2")		
    if (is.null(x$r.squares))
      colnames(res) <- c("B","2.5%","97.5%","SE", "t","p")		
    
    if (x$family == "poisson" || x$family == "nbinomial") {
      OR <- exp(res[, 1:3])
      Q <- (OR - 1) / (OR + 1)
      res <- cbind(res[, -7], round(OR, 3), round(Q, 2))
      colnames(res) <- c(
        "B", "2.5%", "97.5%", "SE", "t", "p", "Odds Ratio", 
        "2.5%", "97.5%","Yule's Q","2.5%", "97.5%"
        )		
    }
    print(res)
    cat("\n")
    cat("Autocorrelations of the residuals\n")
    lag.max = 3
    cr <- acf(residuals(x$full.model), lag.max = lag.max,plot = FALSE)$acf[2:(1 + lag.max)]
    cr <- round(cr, 2)
    print(data.frame(lag = 1:lag.max, cr = cr), row.names = FALSE)
    cat("\n")
    cat("Formula: ")
    print(x$formula, showEnv = FALSE)
    cat("\n")
  }
  
# PAND --------------------------------------------------------------------

  if (value == "PAND") {
    cat("Percentage of all non-overlapping data\n\n")
    cat("PAND = ", round(x$PAND, 1), "%\n")
    cat("\u03A6 = ", round(x$phi, 3), " ; \u03A6\u00b2 = ", round(x$phi^2, 3), "\n\n")
    cat("Number of Cases:", x$N, "\n")
    cat("Total measurements:", x$n, " ")
    cat("(in phase A: ", x$nA, "; in phase B: ", x$nB, ")\n", sep = "")
    cat("n overlapping data per case: ")
    cat(x$OD.PP, sep = ", ")
    cat("\n")
    cat("Total overlapping data: n =",x$OD , "; percentage =", round(x$POD, 1), "\n")
    ma <- x$matrix
    cat("\n")
    cat("2 x 2 Matrix of proportions\n")
    cat("\t% expected\n")
    
    cat("\tA\tB\ttotal\n")
    cat("%    A",round(ma[1, ] * 100, 1), sum(round(ma[1, ] * 100, 1)), sep = "\t")
    cat("\n")
    cat("real B",round(ma[2, ] * 100, 1), sum(round(ma[2, ] * 100, 1)), sep = "\t")
    cat("\n")
    cat(" total",sum(round(ma[, 1] * 100, 1)), sum(round(ma[, 2] * 100, 1)), sep = "\t")
    cat("\n")
    ma <- x$matrix.counts
    cat("\n")
    cat("2 x 2 Matrix of counts\n")
    cat("\texpected\n")
    
    cat("\tA\tB\ttotal\n")
    cat("     A",round(ma[1, ], 1), sum(round(ma[1, ], 1)), sep = "\t")
    cat("\n")
    cat("real B",round(ma[2, ], 1), sum(round(ma[2, ], 1)), sep = "\t")
    cat("\n")
    cat(" total",sum(round(ma[,1], 1)), sum(round(ma[,2 ], 1)), sep = "\t")
    cat("\n")
    cat("\n")
    if (x$correction) cat("\nNote. Matrix is corrected for ties\n")
    cat("\nCorrelation based analysis:\n\n")
    out <- sprintf(
      "z = %.3f, p = %.3f, \u03c4 = %.3f",
      x$correlation$statistic, 
      x$correlation$p.value, 
      x$correlation$estimate
    )
    cat(out, "\n")
  }

# outlier -----------------------------------------------------------------
  
  if (value == "outlier") {
    cat("Outlier Analysis for Single-Case Data\n\n")
    
    if (x$criteria[1] == "CI") {
      names(x$ci.matrix) <- x$case.names
      cat("Criteria: Exceeds", as.numeric(x$criteria[2])*100,"% Confidence Interval\n\n")
      print(x$ci.matrix)
    }
    if (x$criteria[1] == "SD") {
      names(x$sd.matrix) <- x$case.names
      cat("Criteria: Exceeds", x$criteria[2], "Standard Deviations\n\n")
      print(x$sd.matrix)
    }
    if (x$criteria[1] == "MAD") {
      names(x$mad.matrix) <- x$case.names
      cat("Criteria: Exceeds", x$criteria[2], "Mean Average Deviations\n\n")
      print(x$mad.matrix)
    }
    if (x$criteria[1] == "Cook") {
      cat("Criteria: Cook's Distance based on piecewise-linear-regression exceeds", x$criteria[2],"\n\n")
    }
    for(i in 1:length(x$dropped.n)) {
      cat("Case",x$case.names[i],": Dropped",x$dropped.n[[i]],"\n")
    }
    cat("\n")
  }
  
  ##### Additional notes #####
  if (note) .note_vars(x)

  }


#' @rdname print.sc
#' @export
print.sc_ac <- function(x, digits = "auto", ...) {
  
  if (digits == "auto") digits <- 2
  
  cat("Autocorrelations\n\n")
  
  x <- x$autocorr
  for (i in 1:length(x)) {
    x[[i]][, -1] <- round(x[[i]][, -1], digits)
    cat(names(x)[i], "\n")
    print(x[[i]], row.names = FALSE)
    cat("\n")
  }
}

#' @rdname print.sc
#' @export
print.sc_cdc <- function(x, nice = TRUE, ...) {
  
  cat("Conservative Dual Criterion\n\n")
  cat("N cases = ", x$N, "\n\n")
  
  if (nice) x$cdc_p <- .nice.p(x$cdc_p)
  out <- data.frame(
    Case = x$case_names,
    "nB improve" = x$cdc_be,
    "nB" = x$cdc_b,
    "binom p" = x$cdc_p,
    "CDC Evaluation" = x$cdc,
    check.names = FALSE
  )
  print(out, row.names = FALSE)
  cat("\n")
  if (x$decreasing) {
    cat("Assuming an expected decrease in phase B.\n")
    cat("Alternative hypothesis (Binomial test): true probability < 50%\n")
  } else {
    cat("Assuming an expected increase in phase B.\n")
    cat("Alternative hypothesis (Binomial test): true probability > 50%\n")
  }
  if (x$N > 1) {
    cat("Overall evaluation of all MBD instances:  ",x$cdc_all,"\n")
  }

}

#' @rdname print.sc
#' @param nice If set TRUE (default) output values are rounded and optimized for
#' publication tables.
#' @export
print.sc_bctau <- function(x, nice = TRUE, ...) {
  
  cat("Baseline corrected tau\n\n")
  cat("\n")
  
  if (x$continuity) {
    cat("Continuity correction applied\n")
  } else {
    cat("Continuity correction not applied.\n")
  }
  
  if (nice) {
    x$parameters$p <- .nice.p(x$parameters$p)
    x$parameters$z <- sprintf("%.2f", x$parameters$z)
    x$parameters$tau <- sprintf("%.2f", x$parameters$tau)
  }
  
  rownames(x$parameters) <- x$parameters$Model
  print(x$parameters[,-1], ...)

  cat("\n")
  if (x$correction)  cat("Baseline correction should be applied.\n\n")
  if (!x$correction) cat("Baseline correction should not be applied.\n\n")

}

#' @rdname print.sc
#' @param nice If set TRUE (default) output values are rounded and optimized for
#' publication tables.
#' @export
print.sc_desc <- function(x, digits = "auto", ...) {

  if (digits == "auto") digits <- 3
  
  cat("Describe Single-Case Data\n\n")
  x$descriptives[-1:-2] <- round(x$descriptives[-1:-2], digits)
  out <- as.data.frame(t(x$descriptives[-1]))
  colnames(out) <- x$descriptives$Case
  
  print(out[1:(2 * length(x$design) + 1), , drop = FALSE], digits = digits, ...)
  cat("\n")
  print(out[-(1:(2 * length(x$design) + 1)),, drop = FALSE], digits = digits, ...)
  .note_vars(x)

}

#' @rdname print.sc
#' @param nice If set TRUE (default) output values are rounded and optimized for
#' publication tables.
#' @export
print.sc_design <- function(x, ...) {
  cat("A scdf design matrix\n\n")
  cat("Number of cases:", length(x$cases), "\n")
  cat("Mean: ", x$cases[[1]]$m[1], "\n")
  cat("SD = ", x$cases[[1]]$s[1], "\n")
  cat("rtt = ", x$cases[[1]]$rtt[1], "\n")
  cat("Phase design: ", as.character(x$cases[[1]]$phase), "\n")
  
  cat("mean trend-effect: ", apply(sapply(x$cases, function(x) {x$trend}), 1, mean, na.rm = TRUE)[1], "\n")
  cat("mean level-effect: ", apply(sapply(x$cases, function(x) {x$level}), 1, mean, na.rm = TRUE), "\n")
  cat("mean slope-effect: ", apply(sapply(x$cases, function(x) {x$slope}), 1, mean, na.rm = TRUE), "\n")
  cat("sd trend-effect: ", apply(sapply(x$cases, function(x) {x$trend}), 1, sd, na.rm = TRUE)[1], "\n")
  cat("sd level-effect: ", apply(sapply(x$cases, function(x) {x$level}), 1, sd, na.rm = TRUE), "\n")
  cat("sd slope-effect: ", apply(sapply(x$cases, function(x) {x$slope}), 1, sd, na.rm = TRUE), "\n")
  cat("Distribution: ", x$distribution)

}

#' @rdname print.sc
#' @export
print.sc_hplm <- function(x, ...) {
    cat("Hierarchical Piecewise Linear Regression\n\n")
    cat("Estimation method", x$model$estimation.method,"\n")
    cat("Slope estimation method:", x$model$interaction.method,"\n")
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


#' @rdname print.sc
#' @export
print.sc_overlap <- function(x, digits = "auto", ...) {
  
  if (digits == "auto") {
    digits_1 <- 0
    digits_2 <- 2
  } else {
    digits_1 <- digits
    digits_2 <- digits
  }
  
  cat("Overlap Indices\n\n")
  #cat("Design: ", x$design, "\n")
  cat(.stringPhasesSC(x$phases.A, x$phases.B),"\n\n")
  
  x$overlap[3:8] <- round(x$overlap[3:8], digits_1)
  x$overlap[9:14] <- round(x$overlap[9:14], digits_2)
  
  out <- as.data.frame(t(x$overlap[-1]))
  colnames(out) <- x$overlap$Case
  
  print(out, ...)
  .note_vars(x)
}

#' @rdname print.sc
#' @export
print.sc_mplm <- function(x, digits = "auto", std = FALSE, ...) {
  
  if (digits == "auto") digits <- 3
  
  cat("Multivariate piecewise linear model\n\n")
  cat("Dummy model:", x$model, "\n\n")
  
  coef <- x$full.model$coefficients
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
  res$terms <- gsub("(Intercept)", "Intercept", res$terms)
  res$terms <- gsub("mt", "Trend", res$terms)
  res$terms <- gsub("phase", "Level Phase ", res$terms)
  res$terms <- gsub("inter", "Slope Phase ", res$terms)
  
  print(res, digits = digits, ...)
  .note_vars(x)
  
}



.note_vars <- function(x) {
  v <- attr(x, .opt$dv) != "values"
  p <- attr(x, .opt$phase) != "phase"
  m <- attr(x, .opt$mt) != "mt"
  if (v || p || m) { 
    cat("\nThe following variables were used in this analysis:\n'", 
        paste0(attr(x, .opt$dv), collapse = "/ "), "' as dependent variable, '", 
        attr(x, .opt$phase), "' as phase variable,and '", 
        attr(x, .opt$mt),"' as measurement-time variable.\n", sep = "")
  }
}

