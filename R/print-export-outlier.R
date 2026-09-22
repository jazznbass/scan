#' @describeIn outlier Print results
#' @order 2
#' @param x An object returned by [outlier()]
#' @export
#' @inheritParams print.sc
print.sc_outlier <- function(x, digits = "auto", ...) {
  
  cat("Outlier Analysis for Single-Case Data\n\n")
  
  if (identical(x$method, "CI")) {
    names(x$ci.matrix) <- x$case.names
    cat("Criteria: Exceeds", as.numeric(x$criteria) * 100, 
        "% Confidence Interval\n\n")
    print(x$ci.matrix)
  }
  
  if (identical(x$method, "SD")) {
    names(x$sd.matrix) <- x$case.names
    cat("Criteria: Exceeds", x$criteria, "Standard Deviations\n\n")
    print(x$sd.matrix)
  }
  
  if (identical(x$method, "MAD")) {
    names(x$mad.matrix) <- x$case.names
    cat("Criteria: Exceeds", x$criteria, "Median Absolute Deviations\n\n")
    print(x$mad.matrix)
  }
  
  if (identical(x$method, "Cook")) {
    cat("Criteria: Cook's Distance based on piecewise-regression exceeds", 
        x$criteria, "\n\n")
  }
  
  for(i in 1:length(x$dropped.n)) {
    cat("Case", x$case.names[i], ": Dropped", x$dropped.n[[i]], "\n")
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
  
  criterion <- switch(
    object$method,
    "CI"   = paste0("Criterion: exceeds the ", 
                    as.numeric(object$criteria) * 100, "% confidence interval"),
    "SD"   = paste0("Criterion: exceeds ", object$criteria, 
                    " standard deviations"),
    "MAD"  = paste0("Criterion: exceeds ", object$criteria, 
                    " median absolute deviations"),
    "Cook" = paste0("Criterion: Cook's distance based on a piecewise ",
                    "regression exceeds ", object$criteria)
  )
  
  footnote <- .footnote(footnote, criterion)
  
  out <- data.frame(
    Case = object$case.names,
    Dropped = unlist(object$dropped.n),
    "Measurement times" = vapply(
      object$dropped.mt, function(x) paste(x, collapse = ", "), character(1)
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

