#' @rdname export
#' @param nice If set TRUE (default) output values are rounded and optimized for
#' publication tables.
#' @export
export.sc_hplm <- function(object, caption = NA, footnote = NA, filename = NA,
                           kable_styling_options = list(), 
                           kable_options = list(), 
                           round = 2,
                           nice = TRUE,
                           ...) {
  
  kable_options <- .join_kabel(kable_options)
  kable_styling_options <- .join_kabel_styling(kable_styling_options)
  
  if (is.na(caption)) {
    caption <- paste0(
      "Hierarchical Piecewise Linear Regression predicting variable '", 
      attr(object, .opt$dv),  "'"
    )
  }
  kable_options$caption <- caption
  
  summary_model <- summary(object$hplm)
  if (object$model$ICC) {
    ICC <- sprintf(
      "<i>ICC</i> = %.3f, <i>L</i> = %.1f, <i>p</i> = %.3f", 
      object$ICC$value, object$ICC$L, object$ICC$p
    )
  } else {
    ICC <- ""
  }
  
  footnote <- c(
    paste0("Estimation method ", object$model$estimation.method),
    paste0("Slope estimation method: ", object$model$interaction.method),
    paste0(" ", object$model$contrast.method),
    paste0(object$N, " cases")
  )
  footnote <- paste0(footnote, collapse = "; ")
  
  out <- as.data.frame(summary(object$hplm)$tTable)
  
  row.names(out) <- .plm.row.names(row.names(out), object)
  
  colnames(out) <- c("B", "SE", "df", "t", "p")
  
  md <- data.frame(
    "SD" = round(as.numeric(VarCorr(object$hplm)[, "StdDev"]), 3)
  )
  rownames(md) <- names(VarCorr(object$hplm)[, 2])
  
  row.names(md) <- .plm.row.names(row.names(md), object)
  
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
  out <- cbind(Parameter = rownames(out), out, stringsAsFactors = FALSE)
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
  
  out[nrow_out + nrow(md) + 2, 1:2] <- c("AIC", as.character(round(summary_model$AIC, 1)))
  out[nrow_out + nrow(md) + 3, 1:2] <- c("BIC", as.character(round(summary_model$BIC, 1)))
  if (!is.null(object$ICC)) {
    out[nrow_out + nrow(md) + 4, 1:4] <-
      c(
        "ICC", as.character(round(object$ICC$value, 2)),
        paste0("L = ", round(object$ICC$L, 1)),
        paste0("p ", .nice_p(object$ICC$p))
      )
  }
  
  kable_options$x <- out
  kable_options$align <- c("l", rep("r", ncol(out) - 1))
  table <- do.call(kable, kable_options)
  kable_styling_options$kable_input <- table
  table <- do.call(kable_styling, kable_styling_options)
  
  table <- pack_rows(table, "Fixed effects", 1,nrow_out, indent = FALSE)
  table <- pack_rows(table, "Random effects", nrow_out + 1, nrow(out) - 3, indent = FALSE)
  table <- pack_rows(table, "Model", nrow(out) - 2, nrow(out), indent = FALSE)
  #table <- row_spec(table, nrow_out + 1, bold = TRUE, color = "black")
  table <- row_spec(table, nrow_out + nrow(md) + 1, hline_after = TRUE)
  # table <- row_spec(table, nrow(out) - 3, hline_after = TRUE)
  table <- row_spec(table, nrow_out, hline_after = TRUE)
  
  if (!is.na(footnote) && footnote != "") 
    table <- footnote(table, general = footnote, threeparttable = TRUE)
  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) cat(table, file = filename)
  table
  
}
