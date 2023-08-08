#' @rdname export
#' @param nice If set TRUE (default) output values are rounded and optimized for
#' publication tables.
#' @export
export.sc_plm <- function(object, caption = NA, footnote = NA, filename = NA,
                          kable_styling_options = list(), 
                          kable_options = list(), 
                          round = 2,
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

  if (object$ar == 0) out <- summary(object$full.model)$coefficients
  if (object$ar > 0) out <- summary(object$full.model)$tTable
  
  if (nrow(out) == 1) {
    out <- cbind(out[, 1], t(suppressMessages(confint(object$full))), out[, 2:4])
  } else {
    out <- cbind(out[, 1], suppressMessages(confint(object$full)), out[, 2:4])
  }
  out <- as.data.frame(out)
  if (!is.null(object$r.squares)) {
    out$R2 <- c("", format(round(object$r.squares, round)))
  }
  
  row.names(out) <- .plm.row.names(row.names(out), object)
  
  if (!is.null(object$r.squares)) {
    colnames(out) <- c("B", "2.5%", "97.5%", "SE", "t", "p", "Delta R\u00b2")
  }
  if (is.null(object$r.squares)) {
    colnames(out) <- c("B", "2.5%", "97.5%", "SE", "t", "p")
  }
  
  if (object$family == "poisson" || object$family == "nbinomial") {
    OR <- exp(out[, 1:3])
    Q <- (OR - 1) / (OR + 1)
    out <- cbind(out[, -7], round(OR, 3), round(Q, round))
    colnames(out) <- c(
      "B", "2.5%", "97.5%", "SE", "t", "p",
      "Odds Ratio", "2.5%", "97.5%", "Yule's Q", "2.5%", "97.5%"
    )
    
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
  
  out <- cbind(Parameter = rownames(out), out, stringsAsFactors = FALSE)
  rownames(out) <- NULL
  if (object$family == "gaussian") {
    out$B <- sprintf("%.2f", out$B)
    out$"2.5%" <- sprintf("%.2f", out$"2.5%")
    out$"97.5%" <- sprintf("%.2f", out$"97.5%")
    out$SE <- sprintf("%.2f", out$SE)
    out$t <- sprintf("%.2f", out$t)
    if (nice) out$p <- .nice_p((out$p))
    out$"Delta R\u00b2" <- gsub("^0\\.", ".", out$"Delta R\u00b2")
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
  
  table <- .create_table(
    out, 
    kable_options, 
    kable_styling_options, 
    caption = caption,
    footnote = footnote
  )
  
  if (object$family == "gaussian") {
    table <- add_header_above(table, c(" " = 2, "CI(95%)" = 2, " " = 4))
  }
  
  if (object$family %in% c("poisson", "nbinomial")) {
    table <- add_header_above(table, 
                              c(" " = 2, "CI(95%)" = 2, " " = 4, "CI(95%)" = 2," " = 1, "CI(95%)" = 2 )
    )
  }
  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) .save_export(table, filename)
  table
  
}
