#' @rdname print.sc
#' @export
#' 
print.sc_pet <- function(x, ...) {
  cat("Percent Exceeding the Trend\n\n")
  cat("N cases = ", x$N, "\n")
  cat("\n")
  ma <- cbind(x$PET, x$p, x$PET.ci)
  colnames(ma) <- c("PET", "binom.p", "PET CI")
  rownames(ma) <- x$case.names
  print(round(ma, 3))
  cat("\n")
  
  if (x$decreasing) {
    cat("Assumed decreasing values in the B-phase.\n\n")
    cat("Binom.test: alternative hypothesis: true probability < 50%\n")
    cat(sprintf("PET CI: Percent of values less than lower %d%% confidence threshold (smaller %.3f*se below predicted value)\n", x$ci,x$se.factor))
  } else {
    cat("Binom.test: alternative hypothesis: true probability > 50%\n")
    cat(sprintf("PET CI: Percent of values greater than upper %d%% confidence threshold (greater %.3f*se above predicted value)\n", x$ci,x$se.factor))
  }
  
}	

