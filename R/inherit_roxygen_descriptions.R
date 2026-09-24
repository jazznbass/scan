#' Dummy function to inherit global descriptions of parameters
#' 
#' This function is only used to inherit parameter descriptions in the
#' documentation of other functions. It has no other purpose and is not meant to
#' be called directly.
#' 
#' @return No return value.
#' @author Juergen Wilbert
#' @family internal functions
#'
#' @param data A single-case data frame, see [scdf()].
#' @param scdf A single-case data frame, see [scdf()].
#' @param dvar Character string with the name of the dependent variable.
#'   Defaults to the variable set in the scdf.
#' @param pvar Character string with the name of the phase variable. Defaults
#'   to the variable set in the scdf.
#' @param mvar Character string with the name of the measurement time variable.
#'   Defaults to the variable set in the scdf.
#' @param decreasing Set to `TRUE` if the data are expected to be lower in
#'   phase B.
#' @param phases The two phases to compare, by name or by position:
#'   `phases = c("A", "C")` or `phases = c(2, 4)`. A list of two elements
#'   combines phases: `phases = list(A = c(1, 3), B = c(2, 4))` compares phases
#'   1 and 3 against 2 and 4.
#' @param model Model used for the dummy parameters: `"W"`, `"B&L-B"`, `"H-M"`,
#'   or the deprecated `"JW"` (see Huitema & McKean, 2000).
#' @param contrast Sets `contrast_level` and `contrast_slope` at once:
#'   `"first"`, `"preceding"` or a contrast matrix. `NA` leaves both untouched.
#' @param contrast_level `"first"`, `"preceding"` or a contrast matrix. `NA`
#'   takes the value of `contrast`.
#' @param contrast_slope `"first"`, `"preceding"` or a contrast matrix. `NA`
#'   takes the value of `contrast`.
#' @param trend If `TRUE`, the model includes a trend parameter.
#' @param level If `TRUE`, the model includes a level parameter.
#' @param slope If `TRUE`, the model includes a slope parameter.
#' @param nice If `TRUE`, values are rounded and formatted for publication
#'   tables.
#' @param flip If `TRUE`, the table is exported with rows and columns swapped.
#' @param decimals Number of decimal places reported in the table.
#' @param select Names of the variables to include in the table. A named vector
#'   renames them.
#' @param ... Further arguments passed to the function.
#' @keywords internal
.inheritParams <- function(data, scdf, dvar, mvar, pvar, decreasing, 
                           phases, model, contrast, contrast_level, 
                           contrast_slope, trend, level, slope, nice,
                           flip, decimals, select,
                           ...) {
  
} 