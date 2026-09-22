#' @describeIn pand Print results
#' @order 2
#' @param x An object returned by [pand()]
#' @export
#' 
print.sc_pand <- function(x, ...) {
  cat("Percentage of all non-overlapping data\n\n")
  cat("Method:", x$method, "\n\n")
  if (x$decreasing) cat("Expected decrease of phase B scores\n")
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
    ma <- cbind(ma, total = ma[, 1] + ma[, 2])
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
      "X\u00b2 = %.3f, df = 1, p %s",
      x$chi_test$statistic, 
      .nice_p(x$chi_test$p.value, equal.sign = TRUE)
    )
    cat(out, "\n")
    
    cat("\nFisher exact test:\n")
    out <- sprintf(
      "Odds ratio = %.3f, p %s",
      x$fisher_test$estimate, 
      .nice_p(x$fisher_test$p.value, equal.sign = TRUE)
    )
    cat(out, "\n")
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
  
  if (object$method == "sort") {
    footnote <- .footnote(footnote, 
                          paste0("PAND = ", round(object$pand, 1), "%"),
                          "Method: sort",
                          paste0("\u03A6 = ", round(object$phi, 3)), 
                          paste0("\u03A6\u00b2 = ", round(object$phi^2, 3)), 
                          paste0("Number of cases: ", object$N), 
                          sprintf("\u03C7\u00B2 = %.2f, df = 1, p %s; ",
                                  object$chi_test$statistic, 
                                  .nice_p(object$chi_test$p.value, 
                                          equal.sign = TRUE)
                          ),
                          sprintf(
                            "Fisher exact test: Odds ratio = %.2f, p %s",
                            object$fisher_test$estimate, 
                            .nice_p(object$fisher_test$p.value, 
                                    equal.sign = TRUE)
                          ),
                          if (object$decreasing) "Expected decrease of phase B scores" else NULL
    )
    
    
    object$matrix <- rbind(object$matrix, object$matrix[1,] + object$matrix[2,])
    object$matrix_counts <- rbind(
      object$matrix_counts, object$matrix_counts[1,] + object$matrix_counts[2,]
    )
    object$matrix <- cbind(object$matrix, object$matrix[,1] + object$matrix[,2])
    object$matrix_counts <- cbind(
      object$matrix_counts, object$matrix_counts[,1] + object$matrix_counts[,2]
    )  
    out <- as.data.frame(
      round(rbind(object$matrix * 100, object$matrix_counts), round)
    )
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
      align = c("l", "r", "c", "c", "c")
    )
    
    if (.export_engine() == "kable") {
      table <- table  |> 
        add_header_above(c(" " = 2, "Expected" = 3))  |> 
        column_spec(1:2, bold = TRUE)
    }
  }
  
  if (object$method == "minimum") {
    footnote <- .footnote(footnote, 
                          "Method: minimum",
                          if (object$decreasing) "Expected decrease of phase B scores" else NULL
    )
    
    out <- data.frame(
      " " = c(
        "PAND",
        "Overlapping data points",
        "Number of measurements",
        "Number of cases"
      ),
      Value = c(
        round(object$pand, round),
        object$overlaps,
        object$n,
        object$N
      ),
      check.names = FALSE
    )
    
    ops <- options(knitr.kable.NA = "")
    on.exit(options(ops), add = TRUE)
    
    table <- .create_table(
      out, 
      caption = caption,
      footnote = footnote,
      align = c("l", "c")
    )
    
    if (.export_engine() == "kable") {
      table <- table |> column_spec(1, bold = TRUE)
    }
  }
  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) .save_export(table, filename)
  
  table
}

