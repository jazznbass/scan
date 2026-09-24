#' @describeIn rand_test Print results
#' @order 2
#' @param x An object returned by [rand_test()]
#' @export
#' @inheritParams print.sc
print.sc_rand <- function(x, ...) {
  
  out <- .output_rand(x)
  
  cat("Randomization Test\n\n")
  if (out$N > 1) cat("Combined test for", number_word(out$N), "cases.\n\n")
  
  cat(out$phases, "\n")
  
  cat("Statistic: ", out$statistic, "\n\n")
  
  cat(out$design_label, ": ", out$design_value, "\n", sep = "")
  cat("Observed statistic = ", out$observed, "\n")
  
  if (out$auto_corrected) {
    cat("\nWarning! The assigned number of random permutations exceeds the",
        "number of possible permutations.", 
        "\nAnalysis is restricted to all possible permutations.\n")
  }
  
  cat("\n", out$combinations, ".\n", sep = "")
  
  cat("n   = ", out$number, "\n")
  cat("M   = ", out$m, "\n")
  cat("SD  = ", out$sd, "\n")
  cat("Min = ", out$min, "\n")
  cat("Max = ", out$max, "\n")
  cat("\n")
  
  cat("Probability of ", out$direction, ":\n", sep = "")
  
  if (isTRUE(out$p_value == 0)) {
    cat("p   < ", out$p_minimum, "\n")
  } else {
    cat("p   = ", out$p_value, "\n")
  }
  
  dist <- out$distribution
  if (length(dist) > 3 && length(dist) < 5001 && sd(dist) > 0) {
    sh <- shapiro.test(dist)
    cat(sprintf("\nShapiro-Wilk Normality Test: W = %0.3f; p %s", 
                sh[[1]], .nice_p(sh$p.value, equal.sign = TRUE)))
    if (sh$p.value > .05) {
      cat("  (Hypothesis of normality maintained)\n")
    } else {
      cat("  (Hypothesis of normality rejected)\n")
    }
  } else {
    cat("\nA Shapiro-Wilk Test needs between 3 and 5000 finite values",
        "that are not all identical.\n")
  }
  
  cat("\nProbabilty of observed statistic based on the assumption of normality:\n")
  cat(sprintf("z = %0.4f, p = %0.4f (single sided)\n", out$Z, out$p_Z))
  
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
  
  results <- .output_rand(object)
  
  case_names <- attr(object, "casenames")
  footnote <- .footnote(footnote, 
    paste0("N = ", results$N, if (results$N == 1) " case" else " cases"),
    if (!is.null(case_names) && length(case_names) == results$N && 
        all(nzchar(case_names))) {
      paste0(case_names, collapse = ", ")
    },
    results$phases,
    results$combinations,
    paste0("p: probability of ", results$direction)
  )
  
  .nice_value <- function(value) {
    if (length(value) != 1L || !is.finite(value)) return(NA_character_)
    format(round(value, 3))
  }
  
  out <- data.frame(
    Parameter = c(
      "Statistic",
      "Observed statistic",
      results$design_label,
      "Permutations",
      "M of the distribution",
      "SD of the distribution",
      "Min of the distribution",
      "Max of the distribution",
      "p"
    ),
    Value = c(
      results$statistic,
      .nice_value(results$observed),
      results$design_value,
      format(results$number, scientific = FALSE),
      .nice_value(results$m),
      .nice_value(results$sd),
      .nice_value(results$min),
      .nice_value(results$max),
      if (isTRUE(results$p_value == 0)) {
        paste0("< ", results$p_minimum)
      } else if (!is.finite(results$p_value)) {
        NA_character_
      } else {
        format(round(results$p_value, 4), scientific = FALSE)
      }
    ),
    check.names = FALSE
  )
  
  table <- .create_table(
    out,
    caption = caption,
    footnote = footnote,
    ...
  )
  
  if (!is.na(filename)) .save_export(table, filename)
  
  table
  
}

# Values of a rand_test object, extracted once for the print and the export
# method.
.output_rand <- function(x) {
  
  out <- list()
  
  out$statistic      <- x$statistic
  out$observed       <- x$observed.statistic
  out$N              <- x$N
  out$number         <- x$number
  out$complete       <- x$complete
  out$auto_corrected <- x$auto.corrected.number
  out$p_value        <- x$p.value
  out$p_minimum      <- format(1 / x$number, scientific = FALSE)
  out$Z              <- x$Z
  out$p_Z            <- x$p.Z.single
  out$phases         <- .phases_string(x$phases.A, x$phases.B)
  
  # the distribution, reduced to the values that can be summarised
  dist <- x$distribution[is.finite(x$distribution)]
  out$distribution <- dist
  summarise <- function(fun) if (length(dist)) fun(dist) else NA_real_
  out$m   <- summarise(mean)
  out$sd  <- summarise(sd)
  out$min <- summarise(min)
  out$max <- summarise(max)
  
  # how the start of phase B was varied
  if (is.na(x$startpoints[1])) {
    out$design_label <- "Minimal phase length"
    out$design_value <- paste0("A = ", x$limit[1], ", B = ", x$limit[2])
  } else {
    out$design_label <- "Possible starting points of phase B"
    out$design_value <- paste0(x$startpoints, collapse = ", ")
  }
  
  # what the p value is the probability of
  direction <- if (identical(x$testdirection, "greater")) "higher" else "lower"
  out$direction <- paste0(
    if (x$exclude.equal) paste0("a ", direction) else 
      paste0("an equal or ", direction),
    " value than the observed statistic"
  )
  
  out$combinations <- paste0(
    "Distribution based on ",
    if (x$complete) "all " else "a random sample of all ",
    x$possible.combinations, " possible combinations"
  )
  
  out
}
