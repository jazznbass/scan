#' @describeIn hplm Export results as html table (see [export()])
#' @order 3
#' @inheritParams export
#' @param nice If set TRUE (default) output values are rounded and optimized for
#' publication tables.
#' @export
export.sc_hplm <- function(object, caption = NA, footnote = NA, filename = NA,
                           kable_styling_options = list(), 
                           kable_options = list(), 
                           round = 2,
                           nice = TRUE,
                           casewise = FALSE,
                           ...) {
  
  kable_options <- .join_kabel(kable_options)
  kable_styling_options <- .join_kabel_styling(kable_styling_options)
  
  if (is.na(caption)) {
    caption <- paste0(
      "Hierarchical Piecewise Linear Regression predicting variable '", 
      attr(object, opt("dv")),  "'"
    )
  }
  
  footnote <- c(
    paste0("Estimation method ", object$model$estimation.method),
    paste0("Slope estimation method: ", object$model$interaction.method),
    paste0(" ", object$model$contrast.method),
    paste0(object$N, " cases")
  )
  
  if (casewise) {
    out <- .export_casewise(
      object, caption, footnote, filename ,
      kable_styling_options, 
      kable_options, 
      round,
      ...
    )
    return(out)
  }
  
  summary_model <- summary(object$hplm)
  
  out <- as.data.frame(summary(object$hplm)$tTable)
  row.names(out) <- .plm.row.names(row.names(out), object)
  colnames(out) <- c("B", "SE", "df", "t", "p")
  
  md <- data.frame(
    "SD" = round(as.numeric(VarCorr(object$hplm)[, "StdDev"]), 3)
  )
  rownames(md) <- .plm.row.names(names(VarCorr(object$hplm)[, 2]), object)
  
  if (object$model$lr.test) {
    if (is.null(object$LR.test[[1]]$L.Ratio)) {
      object$LR.test[[1]]$L.Ratio <- NA
      object$LR.test[[1]]$"p-value" <- NA
      object$LR.test[[1]]$df <- NA
    }
    
    md$L <- c(
      round(unlist(lapply(object$LR.test, function(x) x$L.Ratio[2])), 2), 
      NA
    )
    md$df <- c(
      unlist(lapply(object$LR.test, function(x) {x$df[2] - x$df[1]})), 
      NA
    )
    md$p <- c(
      round(unlist(lapply(object$LR.test, function(x) x$"p-value"[2])), 3), 
      NA
    )
  }
  
  if (nice) {
    out$p <- .nice_p(out$p)
    if (!is.null(md$p)) md$p <- .nice_p(md$p)
  }
  
  out[, ] <- lapply(out[, ], function(x)
    if (inherits(x, "numeric")) as.character(round(x, round)) else x
  )
  out <- cbind(Predictors = rownames(out), out, stringsAsFactors = FALSE)
  rownames(out) <- NULL
  md[, ] <- lapply(md, function(x)
    if (inherits(x, "numeric")) as.character(round(x, round)) else x
  )
  md <- cbind(" " = rownames(md), md, stringsAsFactors = FALSE)
  rownames(md) <- NULL
  
  nrow_out <- nrow(out)
  tmp_row <- (nrow_out + 1):(nrow_out + nrow(md) + 1 + 3)
  out[tmp_row, ] <- ""
  
  tmp_row <- (nrow_out + 1):(nrow_out + nrow(md) + 1)
  out[tmp_row, 1:ncol(md)] <- rbind(colnames(md), md, stringsAsFactors = FALSE)
  
  out[nrow_out + nrow(md) + 2, 1:2] <- c(
    "AIC", as.character(round(summary_model$AIC, 1))
  )
  out[nrow_out + nrow(md) + 3, 1:2] <- c(
    "BIC", as.character(round(summary_model$BIC, 1))
  )
  if (!is.null(object$ICC)) {
    out[nrow_out + nrow(md) + 4, 1:4] <-
      c(
        "ICC", as.character(round(object$ICC$value, 2)),
        paste0("L = ", round(object$ICC$L, 1)),
        paste0("p ", .nice_p(object$ICC$p))
      )
  }
  
  table <- .create_table(
    out,
    kable_options,
    kable_styling_options,
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
      #row_spec(nrow_out + nrow(md) + 1, hline_after = TRUE) |>
      row_spec(nrow_out, hline_after = TRUE)
  }
      
  if (!is.na(filename)) .save_export(table, filename)
  
  table
  
}

.export_casewise <- function(object, caption = NA, footnote = NA, filename = NA,
                             kable_styling_options = list(), 
                             kable_options = list(), 
                             round = 2,
                             ...) {
  
  out <- coef(object, casewise = TRUE)
  
  if (getOption("scan.export.engine") == "kable") {
    table <- .create_table(
      out,
      kable_options,
      kable_styling_options,
      caption = caption,
      footnote = footnote
    )
  }
  
  if (getOption("scan.export.engine") == "kable") {
    table <- export_table(
      out, title = caption, footnote = footnote, 
      decimals = round
    )
  }

  if (!is.na(filename)) .save_export(table, filename)
  
  table
}
