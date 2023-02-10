#' @rdname export
#' @export
export.sc_trend <- function(object, 
                            caption = NA, 
                            footnote = NA, 
                            filename = NA,
                            kable_styling_options = list(), 
                            kable_options = list(), 
                            round = 2,
                            ...) {
  
  kable_options <- .join_kabel(kable_options)
  kable_styling_options <- .join_kabel_styling(kable_styling_options)
  
  if (is.na(caption)) caption <- c("Trend analysis")
  kable_options$caption <- caption
  
  out <- object$trend
  #if (isTRUE(flip)) out <- t(out)
  
  tmp.rownames <- rownames(out)
  rownames(out) <- NULL
  for (tmp in object$formulas) {
    tmp.rownames <- gsub(paste0(tmp, "."), "", tmp.rownames)
  }
  out <- cbind(Phase = tmp.rownames, out)
  
  kable_options$x <- out
  table <- do.call(kable, kable_options)
  kable_styling_options$kable_input <- table
  table <- do.call(kable_styling, kable_styling_options)
  
  for (i in 1:length(object$formulas)) {
    table <- group_rows(
      table, object$formulas[i],
      1 + (i - 1) * (length(object$design) + 1),
      i * (length(object$design) + 1)
      #label_row_css = "text-align: center;"
    )
  }
  
  if (!is.na(footnote) && footnote != "") 
    table <- footnote(table, general = footnote, threeparttable = TRUE)
  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) cat(table, file = filename)
  table
}
