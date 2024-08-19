#' @rdname export
#' @export
export.sc_pet <- function(object, 
                          caption = NA, 
                          footnote = NA, 
                          filename = NA,
                          kable_styling_options = list(), 
                          kable_options = list(),
                          round = 1,
                          ...) {
  
  kable_options <- .join_kabel(kable_options)
  kable_styling_options <- .join_kabel_styling(kable_styling_options)
  
  if (is.na(caption)) caption <- c("Percent Exceeding the trend")
  
  if (is.na(footnote)) {
    if (object$decreasing) {
      footnote <- c(
        "Assumed decreasing values in the B-phase",
        "Binomial test alternative hypothesis: true probability < 50%",
        "Single-sided test"
      )
    } else {
      footnote <- c(
        "Assumed increasing values in the B-phase",
        "Binomial test alternative hypothesis: true probability > 50%",
        "Single-sided test"
      )
    }
  }
  
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
    kable_options, 
    kable_styling_options, 
    caption = caption,
    footnote = footnote,
    ...
  )
  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) .save_export(table, filename)
  table
}
