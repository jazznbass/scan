#' @rdname export
#' @export
export.sc_nap <- function(object, 
                          caption = NA, 
                          footnote = NA, 
                          filename = NA,
                          kable_styling_options = list(), 
                          kable_options = list(), 
                          select = c("Case", "NAP", "NAP Rescaled", 
                                     "w", "p", "d", "R\u00B2"),
                          ...) {
  
  kable_options <- .join_kabel(kable_options)
  kable_styling_options <- .join_kabel_styling(kable_styling_options)
  
  if (is.na(caption)) caption <- c("Nonoverlap of all pairs")
  
  out <- as.data.frame(object$nap) 
  out <- .select(out, select)
  out$p <- .nice_p(out$p)
  
  table <- .create_table(
    out, 
    kable_options, 
    kable_styling_options, 
    caption = caption,
    footnote = footnote
  )
  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) .save_export(table, filename)
  
  table
}
