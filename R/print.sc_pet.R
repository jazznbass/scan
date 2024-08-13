#' @rdname print.sc
#' @export
#' 
print.sc_pet <- function(x, digits = 3, ...) {
  cat("Percent Exceeding the Trend\n\n")
  cat("\n")
  
  print(x$PET, row.names = FALSE, digits = digits, ...)
  cat("\n")
  
  if (x$decreasing) {
    cat("Assumed decreasing values in the B-phase.\n\n")
    cat("Binom.test: alternative hypothesis: true probability < 50%\n")
    cat(
      sprintf(
        "PET CI: Percent of values less than lower %d%% confidence threshold (single sided)\n", 
        x$ci * 100))
  } else {
    cat("Binom.test: alternative hypothesis: true probability > 50%\n")
    cat(
      sprintf(
        "PET CI: Percent of values greater than upper %d%% confidence threshold (single sided)\n", 
        x$ci * 100))
  }
  
}	

