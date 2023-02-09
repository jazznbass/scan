#' @rdname export
#' @export
export.sc_smd <- function(object, caption = NA, footnote = NA, 
                          filename = NA,
                          select = c("Case", "Mean A" = "mA", "Mean B" = "mB", 
                                     "SD A" =  "sdA", "SD B" = "sdB", 
                                     "SD Cohen" =  "sd cohen", 
                                     "SD Hedges" = "sd hedges", "Glass' delta", 
                                     "Hedges' g", "Hedges' g correction", 
                                     "Hedges' g durlak correction", 
                                     "Cohen's d"),
                          kable_styling_options = list(), 
                          kable_options = list(), 
                          round = 2,
                          flip = FALSE,
                          ...) {
  
  kable_options <- .join_kabel(kable_options)
  kable_styling_options <- .join_kabel_styling(kable_styling_options)
  
  if (is.na(caption)) caption <- c(
    "Standardizes mean differences. ",
    .phases_string(
      object$phases.A, 
      object$phases.B
    )
  )
  
  footnote <- c(
    'SD Cohen = unweigted average of the variance of both phases; ',
    'SD Hedges = weighted average of the variance of both phases with a degrees of freedom correction; ',
    "Glass' delta = mean difference divided by the standard deviation of the A-phase; ",
    "Hedges' g = mean difference divided by SD Hedges; ",
    "Hedges' g (durlak) correction = approaches for correcting Hedges' g for small sample sizes; ",
    "Cohens d = mean difference divided by SD Cohen",
    "."
  )
  footnote <- paste0(footnote, collapse = "")
  caption <- paste0(caption, collapse = "")
  
  kable_options$caption <- caption
  
  out <- object$smd
  
  out <- .select(out, select)
  
  if (isTRUE(flip)) {
    cases <- out[[1]]
    out[-2:-1] <- round(out[-2:-1], round)
    out <- t(out[-1])
    colnames(out) <- cases
  }
  
  kable_options$x <- out
  table <- do.call(kable, kable_options)
  kable_styling_options$kable_input <- table
  table <- do.call(kable_styling, kable_styling_options)
  if (!is.na(footnote) && footnote != "") 
    table <- footnote(table, general = footnote, threeparttable = TRUE)
  table
}
