#' @describeIn rand_test Print results
#' @order 2
#' @param x An object returned by [rand_test()]
#' @export
#' @inheritParams print.sc
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
  #cat("Probability of observed statistic based on distribution:\n")
  
  
  if (x$testdirection == "greater") {
    if (x$exclude.equal) cat("Probability of a higher value than the observed statistic:\n")
    if (!x$exclude.equal) cat("Probability of an equal or higher value than the observed statistic:\n")  
  } else {
    if (x$exclude.equal) cat("Probability of a lower value than the observed statistic:\n")
    if (!x$exclude.equal) cat("Probability of an equal or lower value than the observed statistic:\n")  
  }
  
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

#' @describeIn rand_test Export html results
#' @order 3
#' @inheritParams export
#' @export
export.sc_rand <- function(object, 
                          caption = NA, 
                          footnote = NA, 
                          filename = NA,
                          ...) {
  
  if (is.na(caption)) {
    caption <- paste0(
      "Randomization Test for variable '", 
      attr(object, opt("dv")),  "'"
    )
  }
  
  if (is.na(footnote)) {
  }
  
  out <- capture.output(
    print(object)
  ) [-1:-2]
  
  out <- data.frame(
    "Randomization test" = gt::html(paste(out, collapse = "  <br>")),
    check.names = FALSE
  )
  table <- .create_table(
    out,
    caption = caption,
    footnote = footnote,
    fmt_markdown = TRUE,
    ...
  )
  
  if (!is.na(filename)) .save_export(table, filename)
  
  table
  
}

