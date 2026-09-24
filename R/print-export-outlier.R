#' @describeIn outlier Print results
#' @order 2
#' @param x An object returned by [outlier()]
#' @export
#' @inheritParams print.sc
print.sc_outlier <- function(x, digits = "auto", ...) {
  
  out <- .output_outlier(x)
  
  cat("Outlier Analysis for Single-Case Data\n\n")
  
  cat(out$criterion, "\n\n")
  
  if (!is.null(out$matrix)) print(out$matrix)
  
  for (i in seq_len(nrow(out$dropped))) {
    cat("Case", out$dropped$Case[i], ": Dropped", out$dropped$Dropped[i], "\n")
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
  
  results <- .output_outlier(object)
  
  footnote <- .footnote(footnote, results$criterion)
  
  table <- .create_table(
    results$dropped,
    caption = caption,
    footnote = footnote,
    ...
  )
  
  if (!is.na(filename)) .save_export(table, filename)
  
  table
  
}

# Values of an outlier object, extracted once for the print and the export
# method.
.output_outlier <- function(x) {
  
  out <- list()
  
  out$method   <- x$method
  out$criteria <- x$criteria
  
  out$criterion <- switch(
    x$method,
    "CI"   = paste0("Criterion: exceeds the ", 
                    as.numeric(x$criteria) * 100, "% confidence interval"),
    "SD"   = paste0("Criterion: exceeds ", x$criteria, 
                    " standard deviations"),
    "MAD"  = paste0("Criterion: exceeds ", x$criteria, 
                    " median absolute deviations"),
    "Cook" = paste0("Criterion: Cook's distance based on a piecewise ",
                    "regression exceeds ", x$criteria)
  )
  
  # the bounds the criterion is built from, one table per case
  out$matrix <- switch(
    x$method,
    "CI"  = x$ci.matrix,
    "SD"  = x$sd.matrix,
    "MAD" = x$mad.matrix
  )
  if (!is.null(out$matrix)) names(out$matrix) <- x$case.names
  
  out$dropped <- data.frame(
    Case = x$case.names,
    Dropped = unlist(x$dropped.n),
    "Measurement times" = vapply(
      x$dropped.mt, function(mt) paste(mt, collapse = ", "), character(1)
    ),
    check.names = FALSE
  )
  
  out
}
