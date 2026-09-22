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
  
  dist <- x$distribution[is.finite(x$distribution)]
  cat("n   = ", x$number,"\n")
  cat("M   = ", if (length(dist)) mean(dist) else NA, "\n")
  cat("SD  = ", if (length(dist)) sd(dist) else NA, "\n")
  cat("Min = ", if (length(dist)) min(dist) else NA, "\n")
  cat("Max = ", if (length(dist)) max(dist) else NA, "\n")
  cat("\n")
  #cat("Probability of observed statistic based on distribution:\n")
  
  
  if (x$testdirection == "greater") {
    if (x$exclude.equal) cat("Probability of a higher value than the observed statistic:\n")
    if (!x$exclude.equal) cat("Probability of an equal or higher value than the observed statistic:\n")  
  } else {
    if (x$exclude.equal) cat("Probability of a lower value than the observed statistic:\n")
    if (!x$exclude.equal) cat("Probability of an equal or lower value than the observed statistic:\n")  
  }
  
  if (isTRUE(x$p.value == 0)) {
    cat("p   < ", format(1/x$number, scientific = FALSE), "\n")
  } else {
    cat("p   = ", x$p.value, "\n")
  }
  
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
  
  case_names <- attr(object, "casenames")
  footnote <- .footnote(footnote, 
    paste0("N = ", object$N, if (object$N == 1) " case" else " cases"),
    if (!is.null(case_names) && length(case_names) == object$N && 
        all(nzchar(case_names))) {
      paste0(case_names, collapse = ", ")
    },
    .phases_string(object$phases.A, object$phases.B),
    if (object$complete) {
      paste0("Distribution based on all ", object$possible.combinations, 
             " possible combinations")
    } else {
      paste0("Distribution based on a random sample of all ", 
             object$possible.combinations, " possible combinations")
    },
    if (object$testdirection == "greater") {
      if (object$exclude.equal) {
        "p: probability of a higher value than the observed statistic"
      } else {
        "p: probability of an equal or higher value than the observed statistic"
      }
    } else {
      if (object$exclude.equal) {
        "p: probability of a lower value than the observed statistic"
      } else {
        "p: probability of an equal or lower value than the observed statistic"
      }
    }
  )
  
  dist <- object$distribution[is.finite(object$distribution)]
  
  .nice_value <- function(value) {
    if (length(value) != 1L || !is.finite(value)) return(NA_character_)
    format(round(value, 3))
  }
  
  .dist_stat <- function(fun) {
    if (length(dist) == 0) return(NA_character_)
    .nice_value(fun(dist))
  }
  
  out <- data.frame(
    Parameter = c(
      "Statistic",
      "Observed statistic",
      if (is.na(object$startpoints[1])) "Minimal phase length" else 
        "Possible starting points of phase B",
      "Permutations",
      "M of the distribution",
      "SD of the distribution",
      "Min of the distribution",
      "Max of the distribution",
      "p"
    ),
    Value = c(
      object$statistic,
      .nice_value(object$observed.statistic),
      if (is.na(object$startpoints[1])) {
        paste0("A = ", object$limit[1], ", B = ", object$limit[2])
      } else {
        paste0(object$startpoints, collapse = ", ")
      },
      format(object$number, scientific = FALSE),
      .dist_stat(mean),
      .dist_stat(sd),
      .dist_stat(min),
      .dist_stat(max),
      if (isTRUE(object$p.value == 0)) {
        paste0("< ", format(1 / object$number, scientific = FALSE))
      } else if (!is.finite(object$p.value)) {
        NA_character_
      } else {
        format(round(object$p.value, 4), scientific = FALSE)
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

