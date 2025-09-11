#' @describeIn outlier Print results
#' @order 2
#' @param x An object returned by [outlier()]
#' @export
#' @inheritParams print.sc
print.sc_outlier <- function(x, digits = "auto", ...) {
  
  cat("Outlier Analysis for Single-Case Data\n\n")
  
  if (x$criteria[1] == "CI") {
    names(x$ci.matrix) <- x$case.names
    cat("Criteria: Exceeds", as.numeric(x$criteria[2]) * 100, 
        "% Confidence Interval\n\n")
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
    cat("Criteria: Cook's Distance based on piecewise-regression exceeds", 
        x$criteria[2],"\n\n")
  }
  
  for(i in 1:length(x$dropped.n)) {
    cat("Case",x $case.names[i],": Dropped", x$dropped.n[[i]], "\n")
  }
  cat("\n")
}

#' @describeIn outlier Export html results
#' @order 3
#' @inheritParams export
#' @export
export.sc_outlier <- function(object, 
                              caption = NA, 
                              footnote = NA, 
                              filename = NA,
                              ...) {
  
  if (is.na(caption)) {
    caption <- paste0(
      "Outlier Analysis for variable '", 
      attr(object, opt("dv")),  "'"
    )
  }
  
  if (is.na(footnote)) {
  }
  
  out <- capture.output(
    print(object)
  ) [-1:-2]
  
  out <- data.frame(
    "Outlier Analysis" = gt::html(paste(out, collapse = "  <br>")),
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

