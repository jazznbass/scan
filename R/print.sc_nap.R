#' @rdname print.sc
#' @export
print.sc_nap <- function(x, 
                         digits = "auto", 
                         nice = TRUE, 
                         complete = FALSE, 
                         ...) {
  
  if (digits == "auto") digits <- 2
  cat("Nonoverlap of All Pairs\n\n")
  
  out <- as.data.frame(x$nap)
  if (!complete) out <- out[, -(4:7)]
  if (nice) out$p <- .nice_p(unlist(out$p))
  print(out, digits = digits, row.names = FALSE)
  
}

