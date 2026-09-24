#' Robust improvement rate difference (IRD)
#'
#' Robust improvement rate difference of all cases of a single-case data set.
#' Unlike the other overlap functions, `ird()` returns one value for the whole
#' data set and not one value per case.
#'
#' @details The robust improvement rate difference is derived from the
#'   non-overlapping measurements counted by [pand()] with `method =
#'   "minimum"`, following Pustejovsky (2019):
#'
#'   \deqn{IRD = 1 - \frac{n^2}{2 n_A n_B} \left(1 - PAND\right)}
#'
#'   with \eqn{n_A} and \eqn{n_B} the number of observed measurements in
#'   phase A and in phase B, \eqn{n} their sum, and \eqn{PAND} the percentage
#'   of all non-overlapping data expressed as a proportion. All three counts,
#'   and \eqn{PAND} itself, are taken across all cases at once, so the result is
#'   not the average of the case-wise improvement rate differences. When both
#'   phases are of equal length the factor equals two and the formula reduces
#'   to \eqn{IRD = 2 PAND - 1}; with phases of unequal length it is larger.
#'
#'   For `decreasing = TRUE` the values are mirrored before counting, so that a
#'   drop in phase B counts as an improvement.
#'
#'   Missing values are dropped beforehand and are part of none of the counts.
#'   A case without observed values in one of the phases is removed with a
#'   warning; if no case remains, the function stops with an error.
#' @inheritParams .inheritParams
#' @return An object of class `sc_ird` with the elements:
#'  |  |  |
#'  | --- | --- |
#'  | `ird` | Robust improvement rate difference across all cases. |
#'  | `n_cases` | Number of cases the value is based on. |
#' @author Juergen Wilbert
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
#' @examples
#' ird(exampleAB)
#'
#' # data that are expected to decrease in phase B
#' ird(exampleAB_decreasing, decreasing = TRUE)
#' @order 1
#' @export
ird <- function(data, dvar, pvar,
                decreasing = FALSE,
                phases = c(1, 2)) {
  check_args(
    by_class(decreasing, "logical")
  )
  
  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- dv(data) else dv(data) <- dvar
  if (missing(pvar)) pvar <- phase(data) else phase(data) <- pvar
  
  data <- .prepare_scdf(data)
  recombined_data <- recombine_phases(data, phases = phases)
  
  data <- recombined_data$data
  
  casenames <- revise_names(data)

  pa <- pand(data, method = "minimum", decreasing = decreasing)
  
  ird <- 1 - ( ( (pa$n^2) / (2 * pa$n_a * pa$n_b) ) * (1 - (pa$pand / 100)) )
  
  out <- list(
    ird = ird,
    decreasing = decreasing,
    phases = recombined_data$phases,
    n_cases = pa$N
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
  
  footnote <- .footnote(footnote, 
    if (object$decreasing) "Assumed decreasing values in Phase B"
  )
  
  out <- data.frame("IRD" = round(object$ird, round))
  table <- .create_table(
    out,
    caption = caption,
    footnote = footnote,
    ...
  )
  
  if (!is.na(filename)) .save_export(table, filename)
  
  table

}  

