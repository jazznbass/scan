#' @rdname export
#' @export
export.sc_pet <- function(object, 
                          caption = NA, 
                          footnote = NA, 
                          filename = NA,
                          kable_styling_options = list(), 
                          kable_options = list(),
                          ...) {
  
  kable_options <- .join_kabel(kable_options)
  kable_styling_options <- .join_kabel_styling(kable_styling_options)
  
  if (is.na(caption)) caption <- c("Percent Exceeding the trend")
  
  if (is.na(footnote)) {
    if (object$decreasing) {
      footnote <- c(
        "Assumed decreasing values in the B-phase.",
        "Binom.test: alternative hypothesis: true probability < 50%",
        sprintf(
          "PET CI: Percent of values less than lower %d%% confidence threshold (smaller %.3f*se below predicted value)", 
          object$ci.percent, object$se.factor
        )
      )
    } else {
      footnote <- c(
        "Binom.test: alternative hypothesis: true probability > 50%",
        sprintf(
          "PET CI: Percent of values greater than upper %d%% confidence threshold (greater %.3f*se above predicted value)", 
          object$ci.percent, object$se.factor
        )
      )
    }
  }
  
  table <- .create_table(
    object$PET, 
    kable_options, 
    kable_styling_options, 
    caption = caption,
    footnote = footnote
  )
  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) .save_export(table, filename)
  table
}
