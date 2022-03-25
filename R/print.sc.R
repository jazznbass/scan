#' Print methods for scan objects
#'
#' @param x Object 
#' @param digits The minimum number of significant digits to be use. 
#' If set to "auto" (default), values are predefined.
#' @param ... Further parameters passed to the print function
#' @name print.sc
NULL


#' @rdname print.sc
#' @export
print.sc_nap <- function(x, digits = "auto", ...) {
  
  if (digits == "auto") digits <- 2
  cat("Nonoverlap of All Pairs\n\n")
  print(x$nap, row.names = FALSE, digits = digits)
  
}


#' @rdname print.sc
#' @export
print.sc_outlier <- function(x, digits = "auto", ...) {
  
  cat("Outlier Analysis for Single-Case Data\n\n")
  
  if (x$criteria[1] == "CI") {
    names(x$ci.matrix) <- x$case.names
    cat("Criteria: Exceeds", as.numeric(x$criteria[2]) * 100, "% Confidence Interval\n\n")
    print(x$ci.matrix)
  }
  
  if (x$criteria[1] == "SD") {
    names(x$sd.matrix) <- x$case.names
    cat("Criteria: Exceeds", x$criteria[2], "Standard Deviations\n\n")
    print(x$sd.matrix)
  }
  
  if (x$criteria[1] == "MAD") {
    names(x$mad.matrix) <- x$case.names
    cat("Criteria: Exceeds", x$criteria[2], "Mean Average Deviations\n\n")
    print(x$mad.matrix)
  }
  
  if (x$criteria[1] == "Cook") {
    cat("Criteria: Cook's Distance based on piecewise-linear-regression exceeds", 
        x$criteria[2],"\n\n")
  }
  
  for(i in 1:length(x$dropped.n)) {
    cat("Case",x $case.names[i],": Dropped", x$dropped.n[[i]], "\n")
  }
  cat("\n")
}


#' @rdname print.sc
#' @export
#' 
print.sc_pem <- function(x, ...) {
  cat("Percent Exceeding the Median\n\n")
  ma <- cbind(PEM = x$PEM, x$test)
  print(round(ma, 3))
  cat("\n")
  if (x$decreasing) {
    cat("Assumed decreasing values in the B-phase.\n\n")
    cat("Alternative hypothesis: true probability < 50%\n")
  } else {
    cat("Alternative hypothesis: true probability > 50%\n")
  }
}


#' @rdname print.sc
#' @export
#' 
print.sc_pnd <- function(x, ...) {
  cat("Percent Non-Overlapping Data\n\n")
  out <- data.frame(
    Case = x$case.names, 
    PND = paste0(round(x$PND, 2),"%"), 
    "Total" = x$n.B, 
    "Exceeds" = round(x$PND / 100 * x$n.B)
  )
  print(out, row.names = FALSE)
  cat("\nMean  :", round(mean(x$PND, na.rm = TRUE), 2),"%\n")
}	

#' @rdname print.sc
#' @param complete Print further parameters.
#' @export
#' 
print.sc_tauu <- function(x, complete = FALSE, digits = "auto", ...) {
  
  if (digits == "auto") digits <- 3
  
  cat("Tau-U\n")
  cat("Method:", x$method, "\n")
  cat("Applied Kendall's Tau-", x$tau_method, "\n", sep = "")
  if (complete || (length(x$table) > 1 && x$meta_method != "none")) {
    cat(x$ci * 100, "% CIs for tau are reported.\n\n", sep = "")
  } else cat("\n")
  
  out <- x$table
  
  if (length(out) > 1 && x$meta_method != "none") {
    cat("Overall Tau-U\n")
    cat("Meta-anlysis model:", x$meta_method, "effect\n\n")
    print(x$Overall_tau_u, row.names = FALSE, digits = digits)
    cat("\n")
  }
  
  if (!complete) {
    select_vars <- c("Tau", "SE_Tau", "Z", "p")
    select_rows <- match(
      c(
        "A vs. B", 
        "A vs. B - Trend A",
        "A vs. B + Trend B", 
        "A vs. B + Trend B - Trend A"
      ), row.names(x$table[[1]])
    )
    
    out <- lapply(x$table, function(x) round(x[select_rows, select_vars], 3))
  }
  
  for(i in seq_along(out)) {
    cat("Case:", names(out)[i], "\n")
    print(out[[i]], digits = digits)
    cat("\n")
  }
  
}


#' @rdname print.sc
#' @export
#' 
print.sc_trend <- function(x, digits = 3, ...) {
  x$trend <- round(x$trend, digits)
  cat("Trend for each phase\n\n")
  print(x$trend)
  cat("\n")
  cat("Note. Measurement-times start at", 1 + x$offset, " for each phase\n")
  .note_vars(x)
}

#' @rdname print.sc
#' @export
#' 
print.sc_rci <- function(x, ...) {
  
  cat("Reliable Change Index\n\n")
  cat("Mean Difference = ", x$descriptives[2, 2] - x$descriptives[1, 2], "\n")
  cat("Standardized Difference = ", x$stand.dif, "\n")
  cat("\n")
  cat("Descriptives:\n")
  print(x$descriptives)
  cat("\n")
  cat("Reliability = ", x$reliability, "\n")
  cat("\n")
  cat(x$conf.percent * 100, "% Confidence Intervals:\n")
  print(x$conf)
  cat("\n")
  cat("Reliable Change Indices:\n")
  print(x$RCI)
  cat("\n")
}

.note_vars <- function(x) {
  v <- any(attr(x, .opt$dv) != "values")
  p <- attr(x, .opt$phase) != "phase"
  m <- attr(x, .opt$mt) != "mt"
  if (v || p || m) { 
    cat("\nThe following variables were used in this analysis:\n'", 
        paste0(attr(x, .opt$dv), collapse = "/ "), "' as dependent variable, '", 
        paste0(attr(x, .opt$phase), collapse = "/ "), "' as phase variable, and '", 
        paste0(attr(x, .opt$mt), collapse = "/ "),"' as measurement-time variable.\n", sep = "")
  }
}

