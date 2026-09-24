#' @describeIn cdc Print results
#' @order 2
#' @inheritParams print.sc
#' @inheritParams .inheritParams
#' @export
print.sc_cdc <- function(x, nice = TRUE, ...) {
  
  out <- .output_cdc(x, nice = nice)
  
  cat("Conservative Dual Criterion\n\n")
  cat("N cases = ", out$N, "\n\n")
  
  print(out$table, row.names = FALSE)
  
  cat("\n")
  cat(out$hypothesis, sep = "\n")
  cat("\n")
  if (!is.null(out$overall)) cat(out$overall, "\n")
  
  .note_vars(x)
}

#' @describeIn cdc Export html results
#' @order 3
#' @inheritParams export
#' @inheritParams .inheritParams
#' @export
export.sc_cdc <- function(object, 
                          caption = NA, 
                          footnote = NA, 
                          filename = NA,
                          nice = TRUE,
                          ...) {
  
  
  if (is.na(caption)) {
    caption <- paste0(
      "Conservative Dual Criterion for variable '", 
      attr(object, opt("dv")),  "'"
    )
  }
  
  results <- .output_cdc(object, nice = nice)
  
  footnote <- .footnote(footnote, results$hypothesis, results$overall)
  
  table <- .create_table(
    results$table,
    caption = caption,
    footnote = footnote,
    ...
  )
  
  if (!is.na(filename)) .save_export(table, filename)
  
  table
  
}

# Values of a cdc object, extracted once for the print and the export method.
.output_cdc <- function(x, nice = TRUE) {
  
  out <- list()
  
  out$N          <- x$N
  out$decreasing <- x$decreasing
  out$cdc_all    <- x$cdc_all
  
  p_values <- if (nice) .nice_p(x$cdc_p) else x$cdc_p
  
  out$table <- data.frame(
    Case = x$case_names,
    "nB improve" = x$cdc_exc,
    "nB" = x$cdc_nb,
    "binom p" = p_values,
    "CDC Evaluation" = x$cdc,
    check.names = FALSE
  )
  
  out$hypothesis <- if (x$decreasing) {
    c("Assuming an expected decrease in phase B.",
      "Alternative hypothesis (Binomial test): true probability < 50%")
  } else {
    c("Assuming an expected increase in phase B.",
      "Alternative hypothesis (Binomial test): true probability > 50%")
  }
  
  if (x$N > 1) {
    out$overall <- paste0("Overall evaluation of all MBD instances:  ", x$cdc_all)
  }
  
  out
}
