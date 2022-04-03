#' @rdname print.sc
#' @export
print.sc_nap <- function(x, digits = "auto", ...) {
  
  if (digits == "auto") digits <- 2
  cat("Nonoverlap of All Pairs\n\n")
  print(x$nap, row.names = FALSE, digits = digits)
  
}

