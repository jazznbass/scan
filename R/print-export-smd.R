#' @describeIn smd Print results
#' @order 2
#' @inheritParams print.sc
#' @export
print.sc_smd <- function(x, digits = "auto", ...) {
  
  if (digits == "auto") digits <- 3
  
  cat("Standardized mean differences\n\n")
  x$smd[-1] <- round(x$smd[-1], digits)
  out <- as.data.frame(t(x$smd[-1]))
  colnames(out) <- x$smd$Case
  
  print(out[ , , drop = FALSE], digits = digits, ...)
  cat("\n")
  .note_vars(x)
  
}

#' @describeIn smd Export results as html table (see [export()])
#' @order 3
#' @inheritParams export
#' @inheritParams .inheritParams
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
                          round = 2,
                          decimals = 2,
                          flip = FALSE,
                          ...) {
  
  if (is.na(caption)) caption <- c(
    "Standardizes mean differences. ",
    .phases_string(
      object$phases.A, 
      object$phases.B
    )
  )
  
  footnote <- .footnote(
    footnote,
    'SD Cohen = unweighted average of the variance of both phases',
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
    caption = caption,
    footnote = footnote,
    decimals = decimals,
    ...
  )
  
  if (!is.na(filename)) .save_export(table, filename)
  
  table
}
