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
                          decimals = 2,
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
    'SD Cohen = unweigted average of the variance of both phases',
    'SD Hedges = weighted average of the variance of both phases with a degrees of freedom correction',
    "Glass' delta = mean difference divided by the standard deviation of the A-phase",
    "Hedges' g = mean difference divided by SD Hedges",
    "Hedges' g (durlak) correction = approaches for correcting Hedges' g for small sample sizes",
    "Cohens d = mean difference divided by SD Cohen"
  )
  
  caption <- paste0(caption, collapse = "")
  
  out <- object$smd
  
  out <- .select(out, select)
  
  if (isTRUE(flip)) {
    cases <- out$Case
    out[-1] <- round(out[-1], round)
    names_par <- colnames(out)[-1]
    out <- t(out[-1]) |> as.data.frame()
    out <- cbind(Statistic = rownames(out), out)
    colnames(out) <- c("Statistic", cases)
  }
  
  table <- .create_table(
    out, 
    kable_options, 
    kable_styling_options, 
    caption = caption,
    footnote = footnote,
    decimals = decimals,
    ...
  )
  
  if (!is.na(filename)) .save_export(table, filename)
  
  table
}
