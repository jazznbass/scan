#' @rdname print.sc
#' @export
#' 
print.sc_rand <- function(x, ...) {
  
  cat("Randomization Test\n\n")
  if (x$N > 1) cat("Combined test for", number_word(x$N), "cases.\n\n")
  
  cat(.phases_string(x$phases.A, x$phases.B), "\n")
  
  cat("Statistic: ", x$statistic, "\n\n")
  
  if (is.na(x$startpoints[1])) {
    cat("Minimal length of each phase:", "A =", x$limit[1], ", B =", x$limit[2], "\n")
  } else {
    cat("Possible starting points of phase B: ", x$startpoints, "\n")
  }
  cat("Observed statistic = ", x$observed.statistic, "\n")
  
  if (x$auto.corrected.number) {
    cat("\nWarning! The assigned number of random permutations exceeds the",
        "number of possible permutations.", 
        "\nAnalysis is restricted to all possible permutations.\n")
  }
  if (x$complete) {
    cat("\nDistribution based on all", x$possible.combinations, 
        "possible combinations.\n")
  } else 
    cat("\nDistribution based on a random sample of all", 
        x$possible.combinations, 
        "possible combinations.\n")
  
  cat("n   = ", x$number,"\n")
  cat("M   = ", mean(x$distribution),"\n")
  cat("SD  = ", sd(x$distribution),"\n")
  cat("Min = ", min(x$distribution),"\n")
  cat("Max = ", max(x$distribution),"\n")
  cat("\n")
  cat("Probability of observed statistic based on distribution:\n")
  
  if (x$p.value == 0) {
    cat("p   < ", format(1/x$number, scientific = FALSE), "\n")
  } else {
    cat("p   = ", x$p.value, "\n")
  }
  
  if (x$number > 3 && x$number < 5001) {
    sh <- shapiro.test(x$distribution)
    cat(sprintf("\nShapiro-Wilk Normality Test: W = %0.3f; p = %0.3f",sh[[1]], sh$p.value))
    if (sh$p.value > .05) {
      cat("  (Hypothesis of normality maintained)\n")
    } else {
      cat("  (Hypothesis of normality rejected)\n")
    }
  } else {
    cat("\nSample size must be between 3 and 5000 to perform a Shapiro-Wilk Test.\n")
  }
  
  cat("\nProbabilty of observed statistic based on the assumption of normality:\n")
  cat(sprintf("z = %0.4f, p = %0.4f (single sided)\n", x$Z, x$p.Z.single))
  
}

