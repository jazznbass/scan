#' @rdname print.sc
#' @export
print.sc_smd <- function(x, digits = "auto", ...) {
  
  if (digits == "auto") digits <- 3
  
  cat("Standardized mean differences\n\n")
  x$smd[-1] <- round(x$smd[-1], digits)
  out <- as.data.frame(t(x$smd[-1]))
  colnames(out) <- x$smd$Case
  
  print(out[ , , drop = FALSE], digits = digits, ...)
  cat("\n")
  .note_vars(x)
  
}

