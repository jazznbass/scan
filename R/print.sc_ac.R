#' @rdname print.sc
#' @export
print.sc_ac <- function(x, digits = "auto", ...) {
  
  if (digits == "auto") digits <- 2
  
  cat("Autocorrelations\n\n")
  
  x <- x$autocorr
  for (i in 1:length(x)) {
    x[[i]][, -1] <- round(x[[i]][, -1], digits)
    cat(names(x)[i], "\n")
    print(x[[i]], row.names = FALSE)
    cat("\n")
  }
}
