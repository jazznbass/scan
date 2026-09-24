#' @describeIn pnd Print results
#' @order 2
#' @inheritParams print.sc
#' @export
#' 
print.sc_pnd <- function(x, ...) {
  cat("Percent Non-Overlapping Data\n\n")
  out <- data.frame(
    Case = x$case.names, 
    PND = paste0(round(x$PND, 2),"%"), 
    "Total" = x$n.B, 
    "Exceeds" = round(x$PND / 100 * x$n.B)
  )
  print(out, row.names = FALSE)
  cat("\nMean  :", round(mean(x$PND, na.rm = TRUE), 2),"%\n")
}	

#' @describeIn pnd Export results as html table (see [export()])
#' @order 3
#' @inheritParams export
#' @inheritParams .inheritParams
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
