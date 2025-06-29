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
    paste0("Slope estimation method: ", object$model$interaction.method),
    paste0(" ", object$model$contrast.method),
    paste0(object$N, " cases")
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
