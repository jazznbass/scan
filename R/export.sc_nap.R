#' @rdname export
#' @export
export.sc_nap <- function(object, 
                          caption = NA, 
                          footnote = NA, 
                          filename = NA,
                          select = c("Case", "NAP", "NAP Rescaled", 
                                     "w", "p", "d", "R\u00B2"),
                          round = 2,
                          ...) {
  
  if (is.na(caption)) caption <- c("Nonoverlap of all pairs")
  
  out <- as.data.frame(object$nap) 
  out <- .select(out, select)
  out$p <- .nice_p(out$p)
  
  out <- round_numeric(out, round)
  
  table <- .create_table(
    out, 
    caption = caption,
    footnote = footnote,
    ...
  )
  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) .save_export(table, filename)
  
  table
}
