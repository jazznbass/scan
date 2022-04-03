#' @rdname print.sc
#' @export
print.sc_overlap <- function(x, digits = "auto", ...) {
  
  if (digits == "auto") {
    digits_1 <- 0
    digits_2 <- 2
  } else {
    digits_1 <- digits
    digits_2 <- digits
  }
  
  cat("Overlap Indices\n\n")
  #cat("Design: ", x$design, "\n")
  cat(.phases_string(x$phases.A, x$phases.B),"\n\n")
  
  x$overlap[3:8] <- round(x$overlap[3:8], digits_1)
  x$overlap[9:14] <- round(x$overlap[9:14], digits_2)
  
  out <- as.data.frame(t(x$overlap[-1]))
  colnames(out) <- x$overlap$Case
  
  print(out, ...)
  .note_vars(x)
}
