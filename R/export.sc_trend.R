#' @rdname export
#' @export
export.sc_trend <- function(object, 
                            caption = NA, 
                            footnote = NA, 
                            filename = NA,
                            round = 3,
                            decimals = NULL,
                            ...) {
  
  if (is.na(caption)) caption <- c("Trend analysis")
  if (is.na(footnote)) {
    footnote <- paste(
      "Measurement-times start at", object$first_mt, " for each phase"
    )
  }
  out <- object$trend
  #if (isTRUE(flip)) out <- t(out)
  
  tmp.rownames <- rownames(out)
  rownames(out) <- NULL
  
  for (tmp in names(object$formulas)) {
    tmp.rownames <- gsub(paste0(tmp, "."), "", tmp.rownames)
  }
  out <- cbind(Phase = tmp.rownames, out)
  
  out <- round_numeric(out, round)
  
  row_group <- vector("list", length(object$formulas))
  names(row_group) <- paste0(names(object$formulas), " (", object$formulas,")")
  
  for (i in 1:length(object$formulas)) {
    .start <- 1 + (i - 1) * (length(object$design) + 1)
    row_group[[i]] <- .start : (.start + length(object$design))
  }
  
  table <- .create_table(
    out, 
    caption = caption,
    footnote = footnote,
    row_group = row_group,
    round = round,
    decimals = decimals,
    ...
  )
  
  if (getOption("scan.export.engine") == "kable") {
    for (i in 1:length(object$formulas)) {
      table <- group_rows(
        table, object$formulas[i],
        1 + (i - 1) * (length(object$design) + 1),
        i * (length(object$design) + 1)
        #label_row_css = "text-align: center;"
      )
    }
  }
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) .save_export(table, filename)
  table
}
