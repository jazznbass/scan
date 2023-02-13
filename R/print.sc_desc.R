#' @rdname print.sc
#' @export
print.sc_desc <- function(x, digits = "auto", ...) {
  
  if (digits == "auto") digits <- 3
  
  cat("Describe Single-Case Data\n\n")
  x$descriptives[-1:-2] <- round(x$descriptives[-1:-2], digits)
  out <- as.data.frame(t(x$descriptives[-1]))
  colnames(out) <- x$descriptives$Case
  
  print(
    out[1:(2 * length(x$design) + 1), , drop = FALSE], 
    digits = digits, ...
  )
  cat("\n")
  print(
    out[-(1:(2 * length(x$design) + 1)), , drop = FALSE], 
    digits = digits, ...
  )
  .note_vars(x)
  
}
