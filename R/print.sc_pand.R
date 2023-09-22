#' @describeIn pand Print results
#' @order 2
#' @param x An object returned by [pand()]
#' @export
#' 
print.sc_pand <- function(x, ...) {
  cat("Percentage of all non-overlapping data\n\n")
  cat("Method:", x$method, "\n\n")
  cat("PAND = ", round(x$pand, 1), "%\n", sep = "")

  if (x$method == "sort") {
    cat("\u03A6 = ", round(x$phi, 3), 
        " ; \u03A6\u00b2 = ", 
        round(x$phi^2, 3), "\n\n")
  }
  
  cat(x$n, " measurements (", x$n_a, " Phase A, ", x$n_b, " Phase B) in ", x$N, " cases", sep = "")
  cat("\n")
  cat("Overlapping data: n =",x$overlaps , 
      "; percentage =", round(x$perc_overlap, 1), 
      "\n")
  
  
  if (x$method == "sort") {
    
    ma <- x$matrix_counts
    ma <- cbind(ma, total = ma[1,] + ma[2,])
    ma <- rbind(ma, total = colSums(ma))
    cat("\n")
    cat("2 x 2 Matrix of percentages\n")
    print(round(ma / x$n * 100, 1))

    cat("\n")
    cat("2 x 2 Matrix of counts\n")
    print(round(ma, 1))
    cat("\n")
    cat("\nChi-Squared test:\n")
    out <- sprintf(
      "X\u00b2 = %.3f, df = 1, p = %.3f",
      x$chi_test$statistic, 
      x$chi_test$p.value
    )
    cat(out, "\n")
    
    cat("\nFisher exact test:\n")
    out <- sprintf(
      "Odds ratio = %.3f, p = %.3f",
      x$fisher_test$estimate, 
      x$fisher_test$p.value
    )
    cat(out, "\n")
  }  
}

