#' @rdname print.sc
#' @export
print.sc_nap <- function(x, digits = "auto", nice = TRUE, ...) {
  
  if (digits == "auto") digits <- 2
  cat("Nonoverlap of All Pairs\n\n")
  
  out <- as.data.frame(x$nap)
  row.names(out) <- revise_names(row.names(out))#
  if (nice) out$p <- .nice_p(out$p)
  print(out, digits = digits)
  
}

