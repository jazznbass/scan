#' @rdname export
#' @export
export.sc_trend <- function(object, 
                            caption = NA, 
                            footnote = NA, 
                            filename = NA,
                            kable_styling_options = list(), 
                            kable_options = list(), 
                            round = 2,
                            decimals = 2,
                            ...) {
  
  kable_options <- .join_kabel(kable_options)
  kable_styling_options <- .join_kabel_styling(kable_styling_options)
  
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
  for (tmp in object$formulas) {
    tmp.rownames <- gsub(paste0(tmp, "."), "", tmp.rownames)
  }
  out <- cbind(Phase = tmp.rownames, out)
  
  row_group <- vector("list", length(object$formulas))
  names(row_group) <- object$formulas
  
  for (i in 1:length(object$formulas)) {
    .start <- 1 + (i - 1) * (length(object$design) + 1)
    row_group[[i]] <- .start : (.start + length(object$design))
  }
  
  table <- .create_table(
    out, 
    kable_options, 
    kable_styling_options, 
    caption = caption,
    footnote = footnote,
    row_group = row_group,
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
