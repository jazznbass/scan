#' @describeIn cdc Print results
#' @order 2
#' @inheritParams print.sc
#' @export
print.sc_cdc <- function(x, nice = TRUE, ...) {
  
  cat("Conservative Dual Criterion\n\n")
  cat("N cases = ", x$N, "\n\n")
  
  if (nice) x$cdc_p <- .nice_p(x$cdc_p)
  out <- data.frame(
    Case = x$case_names,
    "nB improve" = x$cdc_be,
    "nB" = x$cdc_b,
    "binom p" = x$cdc_p,
    "CDC Evaluation" = x$cdc,
    check.names = FALSE
  )
  print(out, row.names = FALSE)
  cat("\n")
  if (x$decreasing) {
    cat("Assuming an expected decrease in phase B.\n")
    cat("Alternative hypothesis (Binomial test): true probability < 50%\n")
  } else {
    cat("Assuming an expected increase in phase B.\n")
    cat("Alternative hypothesis (Binomial test): true probability > 50%\n")
  }
  if (x$N > 1) {
    cat("Overall evaluation of all MBD instances:  ",x$cdc_all,"\n")
  }
  
}

#' @describeIn cdc Export html results
#' @order 3
#' @inheritParams export
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
  
  if (is.na(footnote)) {
    if (object$decreasing) {
      footnote <- c(
        "Assuming an expected decrease in phase B.",
        "Alternative hypothesis (Binomial test): true probability < 50%"
      )
    } else {
      footnote <- c(
        "Assuming an expected increase in phase B",
        "Alternative hypothesis (Binomial test): true probability > 50%"
      )
    }
    if (object$N > 1) {
      footnote <- c(
        footnote, 
        paste0("Overall evaluation of all MBD instances:  ",object$cdc_all)
      )
    }
  }
  
  if (nice) object$cdc_p <- .nice_p(object$cdc_p)
  out <- data.frame(
    Case = object$case_names,
    "nB improve" = object$cdc_be,
    "nB" = object$cdc_b,
    "binom p" = object$cdc_p,
    "CDC Evaluation" = object$cdc,
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
