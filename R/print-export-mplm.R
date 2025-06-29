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
  
  out <- .output_mplm(x, std = std)
  
  cat("Multivariate piecewise linear model\n\n")
  cat(
    "Dummy model: ", x$model, " ", 
    paste0("level = ", x$contrast$level, ", slope = " ,x$contrast$slope), "\n", 
    sep = ""
  )
  cat("Type", out$anova$type, "MANOVA", "\n")
  cat(out$fit_string, "\n")
  #" MANOVA Tests: ", out$anova$test, " test statistic", 
  cat("\n")
  out$coef <- round_numeric(out$coef, digits)
  print(out$coef, ...)
  cat("\n")
  if (out$std) cat("Coefficients are standardized\n")
  #cat(out$note, "\n", sep = "")
  cat("Formula: ")
  print(x$formula, showEnv = FALSE)
  
  #.note_vars(x)
}

.output_mplm <- function(x, std) {
  
  out <- list()
  
  out$coef <- if (isTRUE(std)) { 
    x$full.model$coef_std
  } else {
    x$full.model$coefficients
  }
  
  if (inherits(out$coef, "numeric"))  {
    out$coef <- as.data.frame(out$coef)
    names(out$coef) <- attr(x, opt("dv"))
  }
  
  row.names(out$coef) <- rename_predictors(row.names(out$coef), x)
 
  global_test <- anova(x$full.model, x$null_model)
  out$f_test <- c(
    pillai = global_test$Pillai[2],
    df1 = global_test$`num Df`[2],
    df2 = global_test$`den Df`[2],
    f = global_test$`approx F`[2],
    p = global_test$`Pr(>F)`[2]

  )
  out$fit_string <- sprintf(
    "Pillai = %.2f; F(%d, %d) = %.2f; p = %0.3f", 
    out$f_test["pillai"], out$f_test["df1"], out$f_test["df2"], 
    out$f_test["f"], out$f_test["p"]
  )
  ### anova
  
  out$anova <- car::Anova(x$full.model, type = 3)
  
  if (!is.null(out$anova$terms)) {
    out$anova$terms <- rename_predictors(out$anova$terms, x)
  } else {
    row.names(out$anova) <- rename_predictors(row.names(out$anova), x)
  }

  test <- out$anova$test
  repeated <- out$anova$repeated
  ntests <- length(out$anova$terms)
  coef <- matrix(NA, ntests, 4)
  if (!repeated) 
    SSPE.qr <- qr(out$anova$SSPE)
  for (term in 1:ntests) {
    eigs <- Re(
      eigen(
        qr.coef(
          if (repeated) qr(out$anova$SSPE[[term]]) else SSPE.qr, 
          out$anova$SSP[[term]]
        ), 
        symmetric = FALSE
      )$values
    )
    coef[term, 1:4] <- switch(
      test, 
      Pillai = Pillai(eigs, out$anova$df[term], out$anova$error.df), 
      Wilks = Wilks(eigs, out$anova$df[term], out$anova$error.df), 
      `Hotelling-Lawley` = HL(eigs, out$anova$df[term], out$anova$error.df), 
      Roy = Roy(eigs, out$anova$df[term], out$anova$error.df)
    )
  }
  ok <- coef[, 2] >= 0 & coef[, 3] > 0 & coef[, 4] > 0
  ok <- !is.na(ok) & ok
  coef <- cbind(
    out$anova$df, 
    coef, 
    pf(coef[ok, 2], coef[ok, 3], coef[ok, 4], lower.tail = FALSE)
  )
  rownames(coef) <- out$anova$terms
  colnames(coef) <- c(
    "df", "stat", "F", "num Df", "den Df", "p"
  )
  
  coef <- as.data.frame(coef) 
  names(coef)[2] <- test
  
  out$std <- std
  out$coef <- cbind(out$coef, coef[, c(2, 3, 6)]) 
  out$note = paste0(
    "df = ", round(coef[1, 1], 1), 
    "; num Df = ", coef[1, 4], 
    "; den Df = ", coef[1, 5]
  )

  out
  
}

Roy <- function (eig, q, df.res) {
  p <- length(eig)
  test <- max(eig)
  tmp1 <- max(p, q)
  tmp2 <- df.res - tmp1 + q
  c(test, (tmp2 * test)/tmp1, tmp1, tmp2)
}

HL <- function (eig, q, df.res) {
  test <- sum(eig)
  p <- length(eig)
  m <- 0.5 * (abs(p - q) - 1)
  n <- 0.5 * (df.res - p - 1)
  s <- min(p, q)
  tmp1 <- 2 * m + s + 1
  tmp2 <- 2 * (s * n + 1)
  c(test, (tmp2 * test)/s/s/tmp1, s * tmp1, tmp2)
}

Wilks <- function (eig, q, df.res) {
  test <- prod(1/(1 + eig))
  p <- length(eig)
  tmp1 <- df.res - 0.5 * (p - q + 1)
  tmp2 <- (p * q - 2)/4
  tmp3 <- p^2 + q^2 - 5
  tmp3 <- if (tmp3 > 0) 
    sqrt(((p * q)^2 - 4)/tmp3)
  else 1
  c(test, ((test^(-1/tmp3) - 1) * (tmp1 * tmp3 - 2 * tmp2))/p/q, 
    p * q, tmp1 * tmp3 - 2 * tmp2)
}

Pillai <- function (eig, q, df.res) {
  test <- sum(eig/(1 + eig))
  p <- length(eig)
  s <- min(p, q)
  n <- 0.5 * (df.res - p - 1)
  m <- 0.5 * (abs(p - q) - 1)
  tmp1 <- 2 * m + s + 1
  tmp2 <- 2 * n + s + 1
  c(test, (tmp2/tmp1 * test)/(s - test), s * tmp1, s * tmp2)
}

#' @describeIn mplm Export results as html
#' @inheritParams export
#' @param std If TRUE, a table with standardized estimates is included.
#' @export
export.sc_mplm <- function(object, 
                          caption = NA, 
                          footnote = NA, 
                          filename = NA,
                          nice = TRUE,
                          std = FALSE,
                          decimals = 2,
                          ...) {
  

  if (is.na(caption)) {
    caption <- "Multivariate Piecewise-Regression Model"
      #"Multivariate Piecewise-regression model predicting '", 
      #paste0(attr(object, opt("dv")), collapse = "/"), "'"
    #)
  }
  
  results <- .output_mplm(object, std = std)
  
  out <- results$coef
  out <- rownames_to_first_column(out, "Parameter")
  
  if (nice) out$p <- .nice_p(out$p)
  
  if (is.na(footnote)) footnote <- c(
    paste0(results$fit_string)
  )
  
  if (std) {
    footnote <- c(footnote, "Predictors are standardized")
  }
  
  n_dv <- length(attr(object, opt("dv")))
  
  if (getOption("scan.export.engine") == "gt") {
    spanner <- list("Dependent variables" = 2:(1 + n_dv))
  }
  
  out <- round_numeric(out, decimals)
  table <- .create_table(
    out, 
    caption = caption,
    footnote = footnote,
    spanner = spanner
  )
  
  if (getOption("scan.export.engine") == "kable") {
    spanner <- c(" " = 1, "Dependent variables" = n_dv, " " = 3)
    table <- add_header_above(table, spanner)
  }
    
 
    
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) .save_export(table, filename)
  table
  
}
