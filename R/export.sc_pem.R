#' @rdname export
#' @export
export.sc_pem <- function(object, 
                          caption = NA, 
                          footnote = NA, 
                          filename = NA,
                          kable_styling_options = list(), 
                          kable_options = list()) {
  
  kable_options <- .join_kabel(kable_options)
  kable_styling_options <- .join_kabel_styling(kable_styling_options)
  
  if (is.na(caption)) caption <- c("Percent Exceeding the Median")
  
  table <- .create_table(
    object$PEM, 
    kable_options, 
    kable_styling_options, 
    caption = caption,
    footnote = footnote
  )
  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) .save_export(table, filename)
  table
}
