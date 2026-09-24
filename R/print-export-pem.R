#' @describeIn pem Print results
#' @order 2
#' @inheritParams print.sc
#' @export
#' 
print.sc_pem <- function(x, ...) {
  cat("Percent Exceeding the Median\n\n")
  print(x$PEM, digits = 3, row.names = FALSE)
  cat("\n")
  if (x$decreasing) {
    cat("Assumed decreasing values in the B-phase.\n\n")
    cat("Alternative hypothesis: true probability < 50%\n")
  } else {
    cat("Alternative hypothesis: true probability > 50%\n")
  }
}

#' @describeIn pem Export results as html table (see [export()])
#' @order 3
#' @inheritParams export
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
