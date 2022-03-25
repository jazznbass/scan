#' @rdname print.sc
#' @export
#' 
print.sc_pand <- function(x, ...) {
  cat("Percentage of all non-overlapping data\n\n")
  cat("PAND = ", round(x$pand, 1), "%\n")
  cat("\u03A6 = ", round(x$phi, 3), 
      " ; \u03A6\u00b2 = ", 
      round(x$phi^2, 3), "\n\n")
  cat("Number of cases:", x$N, "\n")
  cat("Total measurements:", x$n, " ")
  cat("(in phase A: ", x$nA, "; in phase B: ", x$nB, ")\n", sep = "")
  cat("n overlapping data per case: ")
  cat(x$overlaps_cases, sep = ", ")
  cat("\n")
  cat("Total overlapping data: n =",x$overlaps , 
      "; percentage =", round(x$perc_overlap, 1), 
      "\n")
  ma <- x$matrix
  cat("\n")
  cat("2 x 2 Matrix of proportions\n")
  cat("\t% expected\n")
  
  cat("\tA\tB\ttotal\n")
  cat("%    A",round(ma[1, ] * 100, 1), sum(round(ma[1, ] * 100, 1)), sep = "\t")
  cat("\n")
  cat("real B",round(ma[2, ] * 100, 1), sum(round(ma[2, ] * 100, 1)), sep = "\t")
  cat("\n")
  cat(" total",sum(round(ma[, 1] * 100, 1)), sum(round(ma[, 2] * 100, 1)), sep = "\t")
  cat("\n")
  ma <- x$matrix_counts
  cat("\n")
  cat("2 x 2 Matrix of counts\n")
  cat("\texpected\n")
  
  cat("\tA\tB\ttotal\n")
  cat("     A",round(ma[1, ], 1), sum(round(ma[1, ], 1)), sep = "\t")
  cat("\n")
  cat("real B",round(ma[2, ], 1), sum(round(ma[2, ], 1)), sep = "\t")
  cat("\n")
  cat(" total",sum(round(ma[,1], 1)), sum(round(ma[,2 ], 1)), sep = "\t")
  cat("\n")
  cat("\n")
  if (x$correction) cat("\nNote. Matrix is corrected for ties\n")
  cat("\nCorrelation based analysis:\n\n")
  out <- sprintf(
    "z = %.3f, p = %.3f, \u03c4 = %.3f",
    x$correlation$statistic, 
    x$correlation$p.value, 
    x$correlation$estimate
  )
  cat(out, "\n")
}

