#' @rdname print.sc
#' @param duration If TRUE the duration for computation is printed.
#' @export
print.sc_power <- function(x, duration = FALSE, digits = 3, ...) {
  
  cat("Test-Power in percent:\n\n")
  
  class(x) <- "data.frame"
  print(x,row.names = FALSE, digits = digits)
  if (duration) 
    cat(
      "\nComputation duration is", 
      round(attr(x, "computation_duration")[3], 1), 
      "seconds.\n"
    )
  
}
