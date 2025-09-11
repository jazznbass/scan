#' IRD - Improvement rate difference
#'
#' `ird()` calculates the robust improvement rate difference as proposed by
#' Parker and colleagues (2011).
#'
#' The adaptation of the improvement rate difference for single-case phase
#' comparisons was developed by Parker and colleagues (2009). A variation called
#' robust improvement rate difference was proposed by Parker and colleagues in
#' 2011. This function calculates the robust improvement rate difference. It
#' follows the formula suggested by Pustejovsky (2019). For a multiple case
#' design, ird is based on the overall improvement rate of all cases which is
#' the average of the irds for each case.
#' @inheritParams .inheritParams
#' @family overlap functions
#' @references Parker, R. I., Vannest, K. J., & Brown, L. (2009). The
#'   improvement rate difference for single-case research. Exceptional Children,
#'   75(2), 135-150.
#'
#'   Parker, R. I., Vannest, K. J., & Davis, J. L. (2011). Effect Size in
#'   Single-Case Research: A Review of Nine Nonoverlap Techniques. Behavior
#'   Modification, 35(4), 303-322. https://doi.org/10.1177/0145445511399147
#'
#'   Pustejovsky, J. E. (2019). Procedural sensitivities of effect sizes for
#'   single-case designs with directly observed behavioral outcome measures.
#'  *Psychological Methods*, *24(2)*, 217-235.
#'   https://doi.org/10.1037/met0000179
#'
#' @order 1
#' @export
ird <- function(data, dvar, pvar,
                decreasing = FALSE,
                phases = c(1, 2)) {
  
  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- dv(data) else dv(data) <- dvar
  if (missing(pvar)) pvar <- phase(data) else phase(data) <- pvar
  
  data <- .prepare_scdf(data, na.rm = TRUE)
  recombined_data <- recombine_phases(data, phases = phases)
  
  data <- recombined_data$data
  
  casenames <- revise_names(data)

  pa <- pand(data, method = "minimum", decreasing = decreasing)
  
  ird <- 1 - ( ( (pa$n^2) / (2 * pa$n_a * pa$n_b) ) * (1 - (pa$pand / 100)) )
  
  out <- list(
    ird = ird,
    decreasing = decreasing,
    phases = recombined_data$phases,
    n_cases = length(data)
  )
  
  class(out) <- "sc_ird"
  attributes(out)[opts("phase", "dv")] <- list(pvar, dvar)
  out
  
}


#' @describeIn ird Print results
#' @order 2
#' @param x An object returned by [ird()]
#' @param digits The minimum number of significant digits to be use. 
#' @export
#' 
print.sc_ird <- function(x, digits = 3, ...) {
  cat("Improvement rate difference =",  round(x$ird, digits))
  if (x$decreasing) {
    cat("\nAssumed decreasing values in Phase B.\n\n")
  }
}

#' @describeIn ird Export results to html
#' @order 3
#' @inheritParams export
#' @export
export.sc_ird <- function(object, 
                           caption = NA, 
                           footnote = NA, 
                           filename = NA,
                           round = 3,
                           ...) {
  
  if (is.na(caption)) {
    caption <- paste0(
      "Improvement rate difference for variable '", 
      attr(object, opt("dv")),  "'"
    )
  }
  
  if (is.na(footnote)) {
    if (object$decreasing) {
      footnote <- "Assumed decreasing values in Phase B"
    }
  }
  
  
  out <- data.frame("IRD" = round(object$ird, round))
  table <- .create_table(
    out,
    caption = caption,
    footnote = footnote,
    ...
  )
  table

}  
