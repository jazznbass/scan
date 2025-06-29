#' @rdname export
#' @export
export.sc_pem <- function(object, 
                          caption = NA, 
                          footnote = NA, 
                          filename = NA,
                          round = 2,
                          ...) {
  
  if (is.na(caption)) caption <- c("Percent Exceeding the Median")
  
  object$PEM <- round_numeric(object$PEM, round)
  
  table <- .create_table(
    object$PEM, 
    caption = caption,
    footnote = footnote,
    ...
  )
  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) .save_export(table, filename)
  table
}
