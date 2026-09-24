#' @describeIn pet Print results
#' @order 2
#' @inheritParams print.sc
#' @export
#' 
print.sc_pet <- function(x, digits = 3, ...) {
  cat("Percent Exceeding the Trend\n\n")
  cat("\n")
  
  print(x$PET, row.names = FALSE, digits = digits, ...)
  cat("\n")
  
  if (x$decreasing) {
    cat("Assumed decreasing values in the B-phase.\n\n")
    cat("Binom.test: alternative hypothesis: true probability < 50%\n")
    cat(
      sprintf(
        "PET CI: Percent of values less than lower %d%% confidence threshold (single sided)\n", 
        x$ci * 100))
  } else {
    cat("Binom.test: alternative hypothesis: true probability > 50%\n")
    cat(
      sprintf(
        "PET CI: Percent of values greater than upper %d%% confidence threshold (single sided)\n", 
        x$ci * 100))
  }
  
}	

#' @describeIn pet Export results as html table (see [export()])
#' @order 3
#' @inheritParams export
#' @export
export.sc_pet <- function(object, 
                          caption = NA, 
                          footnote = NA, 
                          filename = NA,
                          round = 1,
                          ...) {
  
  if (is.na(caption)) caption <- c("Percent Exceeding the trend")
  
  footnote <- .footnote(footnote,
    if (object$decreasing) {
      c("Assumed decreasing values in the B-phase",
        "Binomial test alternative hypothesis: true probability < 50%",
        "Single-sided test")
    } else {
      c("Assumed increasing values in the B-phase",
        "Binomial test alternative hypothesis: true probability > 50%",
        "Single-sided test")
    }
  )
  
  object$PET$binom.p <- .nice_p(object$PET$binom.p)
  names(object$PET)[4] <- "p (binomial test)"
  
  new <- paste0(
    "Percentage ", if (object$decreasing) "< lower " else "> upper ", 
    object$ci * 100, " CI"
  )
  names(object$PET)[3] <- new
  object$PET <- round_numeric(object$PET, round)
  
  table <- .create_table(
    object$PET, 
    caption = caption,
    footnote = footnote,
    ...
  )
  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) .save_export(table, filename)
  table
}
