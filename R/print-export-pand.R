#' @describeIn pand Print results
#' @order 2
#' @param x An object returned by [pand()]
#' @export
#' 
print.sc_pand <- function(x, ...) {
  
  out <- .output_pand(x)
  
  cat("Percentage of all non-overlapping data\n\n")
  cat("Method:", out$method, "\n\n")
  if (out$decreasing) cat("Expected decrease of phase B scores\n")
  cat("PAND = ", round(out$pand, 1), "%\n", sep = "")

  if (identical(out$method, "sort")) {
    cat("\u03A6 = ", round(out$phi, 3), 
        " ; \u03A6\u00b2 = ", round(out$phi_squared, 3), "\n\n")
  }
  
  cat(out$n, " measurements (", out$n_a, " Phase A, ", out$n_b, " Phase B) in ", 
      out$N, " cases", sep = "")
  cat("\n")
  cat("Overlapping data: n =", out$overlaps, 
      "; percentage =", round(out$perc_overlap, 1), "\n")
  
  if (identical(out$method, "sort")) {
    
    cat("\n")
    cat("2 x 2 Matrix of percentages\n")
    print(round(out$matrix_percent, 1))

    cat("\n")
    cat("2 x 2 Matrix of counts\n")
    print(round(out$matrix_counts, 1))
    
    cat("\n")
    cat("\nChi-Squared test:\n")
    cat(.pand_chi_line(out), "\n")
    
    cat("\nFisher exact test:\n")
    cat(.pand_fisher_line(out), "\n")
  }  
}

#' @describeIn pand Export results as html table (see [export()])
#' @inheritParams export
#' @order 3
#' @export
export.sc_pand <- function(object, 
                           caption = NA, 
                           footnote = NA, 
                           filename = NA,
                           round = 1,
                           ...) {
  
  if (is.na(caption)) {
    caption <- c("Percentage of all non-overlapping data (PAND)")
  }
  
  results <- .output_pand(object)
  
  if (identical(results$method, "sort")) {
    footnote <- .footnote(footnote, 
                          paste0("PAND = ", round(results$pand, 1), "%"),
                          "Method: sort",
                          paste0("\u03A6 = ", round(results$phi, 3)), 
                          paste0("\u03A6\u00b2 = ", round(results$phi_squared, 3)), 
                          paste0("Number of cases: ", results$N), 
                          paste0(.pand_chi_line(results), "; "),
                          paste0("Fisher exact test: ", .pand_fisher_line(results)),
                          if (results$decreasing) 
                            "Expected decrease of phase B scores" else NULL
    )
    
    out <- as.data.frame(round(
      rbind(results$matrix_percent, results$matrix_counts), round
    ))
    out <- cbind(
      data.frame(
        " " = rep(c("Real", " ", " "), 2), Phase = rep(c("A", "B", "Total"), 2)
      ), 
      out
    )
    names(out) <- c(" ", "  ", "A", "B", "Total")
    
    ops <- options(knitr.kable.NA = "")
    on.exit(options(ops), add = TRUE)
    
    table <- .create_table(
      out, 
      caption = caption,
      footnote = footnote,
      spanner = list("Expected" = 3:5),
      row_group = list("Percentage" = 1:3, "Counts" = 4:6),
      align = c("l", "r", "c", "c", "c"),
      bold_columns = 1:2
    )
  }
  
  if (identical(results$method, "minimum")) {
    footnote <- .footnote(footnote, 
                          "Method: minimum",
                          if (results$decreasing) 
                            "Expected decrease of phase B scores" else NULL
    )
    
    out <- data.frame(
      " " = c(
        "PAND",
        "Overlapping data points",
        "Number of measurements",
        "Number of cases"
      ),
      Value = c(
        round(results$pand, round),
        results$overlaps,
        results$n,
        results$N
      ),
      check.names = FALSE
    )
    
    ops <- options(knitr.kable.NA = "")
    on.exit(options(ops), add = TRUE)
    
    table <- .create_table(
      out, 
      caption = caption,
      footnote = footnote,
      align = c("l", "c"),
      bold_columns = 1
    )
  }
  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) .save_export(table, filename)
  
  table
}

# Values of a pand object, extracted once for the print and the export method.
.output_pand <- function(x) {
  
  out <- list()
  
  out$method       <- x$method
  out$decreasing   <- x$decreasing
  out$pand         <- x$pand
  out$n            <- x$n
  out$n_a          <- x$n_a
  out$n_b          <- x$n_b
  out$N            <- x$N
  out$overlaps     <- x$overlaps
  out$perc_overlap <- x$perc_overlap
  
  if (identical(x$method, "sort")) {
    
    out$phi         <- x$phi
    out$phi_squared <- x$phi^2
    
    # the two by two matrices with their row and column totals
    with_totals <- function(m) {
      m <- cbind(m, Total = m[, 1] + m[, 2])
      rbind(m, Total = colSums(m))
    }
    out$matrix_counts  <- with_totals(x$matrix_counts)
    out$matrix_percent <- with_totals(x$matrix) * 100
    
    out$chi    <- x$chi_test
    out$fisher <- x$fisher_test
  }
  
  out
}

# The two test results as one line each, identical in print and export.
.pand_chi_line <- function(out) {
  sprintf(
    "\u03C7\u00B2 = %.3f, df = 1, p %s",
    out$chi$statistic, .nice_p(out$chi$p.value, equal.sign = TRUE)
  )
}

.pand_fisher_line <- function(out) {
  sprintf(
    "Odds ratio = %.3f, p %s",
    out$fisher$estimate, .nice_p(out$fisher$p.value, equal.sign = TRUE)
  )
}
