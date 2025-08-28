#' @describeIn hplm Print results
#' @inheritParams print.sc
#' @param casewise Returns the effect estimations for each case
#' @param bcsmd If TRUE, reports between-case standardized mean differences.
#' @order 2
#' @param x An object returned by [hplm()]
#' @export
print.sc_hplm <- function(x, 
                          digits = 3, 
                          bcsmd = FALSE, 
                          casewise = FALSE,
                          ...) {
  
  out <- .output_hplm(x, casewise = casewise, bcsmd = bcsmd)
  
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

#' @describeIn hplm Export results as html table (see [export()])
#' @order 3
#' @inheritParams export
#' @export
export.sc_hplm <- function(object, 
                           caption = NA, 
                           footnote = NA, 
                           filename = NA,
                           round = 2,
                           nice = TRUE,
                           casewise = FALSE,
                           ...) {
  
  if (is.na(caption)) {
    caption <- paste0(
      "Hierarchical Piecewise Linear Regression predicting variable '", 
      attr(object, opt("dv")),  "'"
    )
  }
  
  footnote <- c(
    paste0("Estimation method ", object$model$estimation.method),
    str_contrasts(object$model$interaction.method, object$contrast),
    paste0("N = ", object$N, " cases")
  )
  
  if (casewise) {
    out <- .export_casewise(object, caption, footnote, filename, round)
    return(out)
  }
  
  results <- .output_hplm(object)
  
  out <- results$fixed
  dat_random <- results$random
  
  if (nice) {
    out$p <- .nice_p(out$p)
    if (!is.null(dat_random$p)) dat_random$p <- .nice_p(dat_random$p)
  }
  
  out[, ] <- lapply(out[, ], function(x)
    if (inherits(x, "numeric")) as.character(round(x, round)) else x
  )
  out <- cbind(Predictors = rownames(out), out, stringsAsFactors = FALSE)
  rownames(out) <- NULL
  dat_random[, ] <- lapply(dat_random, function(x)
    if (inherits(x, "numeric")) as.character(round(x, round)) else x
  )
  dat_random <- cbind(
    " " = rownames(dat_random), 
    dat_random, 
    stringsAsFactors = FALSE
  )
  rownames(dat_random) <- NULL
  
  nrow_out <- nrow(out)
  nrow_random <- nrow(dat_random)
  tmp_row <- (nrow_out + 1):(nrow_out + nrow_random + 1 + 3)
  out[tmp_row, ] <- ""
  
  tmp_row <- (nrow_out + 1):(nrow_out + nrow_random + 1)
  out[tmp_row, 1:ncol(dat_random)] <- rbind(
    colnames(dat_random), 
    dat_random, 
    stringsAsFactors = FALSE
  )
  
  out[nrow_out + nrow_random + 2, 1:2] <- c(
    "AIC", as.character(round(results$AIC, 1))
  )
  out[nrow_out + nrow_random + 3, 1:2] <- c(
    "BIC", as.character(round(results$BIC, 1))
  )
  if (!is.null(object$ICC)) {
    out[nrow_out + nrow_random + 4, 1:4] <-
      c(
        "ICC", 
        as.character(round(object$ICC$value, 2)),
        paste0("L = ", round(object$ICC$L, 1)),
        paste0("p ", .nice_p(object$ICC$p))
      )
  }
  
  table <- .create_table(
    out,
    caption = caption,
    footnote = footnote,
    row_group = list(
      "Fixed effects" = 1: nrow_out,
      "Random effects" = (nrow_out + 1) : (nrow(out) - 3),
      "Model" = (nrow(out) - 2) : nrow(out)
    )
  )
  
  if (getOption("scan.export.engine") == "kable") {
    table <- table |>
      #pack_rows("Fixed effects", 1, nrow_out, indent = FALSE) |>
      pack_rows("\nRandom effects", nrow_out + 1, nrow(out), indent = FALSE) |>
      pack_rows("\nModel", nrow(out) - 2, nrow(out), indent = FALSE) |>
      #row_spec(nrow_out + nrow(dat_random) + 1, hline_after = TRUE) |>
      row_spec(nrow_out, hline_after = TRUE)
  }
  
  if (!is.na(filename)) .save_export(table, filename)
  
  table
  
}

.export_casewise <- function(object, 
                             caption = NA, 
                             footnote = NA, 
                             filename = NA,
                             round = 2) {
  
  out <- coef(object, casewise = TRUE)
  
  if (getOption("scan.export.engine") == "kable") {
    table <- .create_table(
      out,
      caption = caption,
      footnote = footnote
    )
  }
  
  if (getOption("scan.export.engine") == "gt") {
    table <- export_table_gt(
      out, title = caption, footnote = footnote, 
      decimals = round
    )
  }
  
  if (!is.na(filename)) .save_export(table, filename)
  
  table
}


.output_hplm <- function(x, casewise = FALSE, bcsmd = FALSE) {
  
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
  cor_matrix <- cor_matrix[-1,-ncol(cor_matrix), drop = FALSE]
 
  if (nrow(cor_matrix) > 0) out$correlation <- cor_matrix
  
  # casewise ------
  
  if (casewise) out$casewise <- coef(x, casewise = TRUE)
  
  # bcsmd ----
  
  if (bcsmd) {
    out$bc_smd <- between_smd(x)$models[[1]]
  }
  
  out
  
}