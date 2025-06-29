#' @rdname export
#' @export
export.sc_pnd <- function(object, 
                          caption = NA, 
                          footnote = NA, 
                          filename = NA,
                          select = c("Case", "PND", "Total", "Exceeds"),
                          round = 2,
                          ...) {
  
  if (is.na(caption)) caption <- c("Percentage Non-Overlapping Data")
  
  out <- data.frame(
    Case = object$case.names, 
    PND = object$PND, 
    Total = object$n.B, 
    Exceeds = round(object$n.B * object$PND / 100)
  )
  out <- .select(out, select)
  
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
