#' Dummy function to inherit global descriptions of parameters
#'
#' @param data A single-case data frame. See [scdf()] to learn about this
#'   format.
#' @param scdf A single-case data frame. See [scdf()] to learn about this
#'   format.
#' @param dvar Character string with the name of the dependent variable.
#'   Defaults to the attributes in the scdf file.
#' @param pvar Character string with the name of the phase variable. Defaults to
#'   the attributes in the scdf file.
#' @param mvar Character string with the name of the measurement time variable.
#'   Defaults to the attributes in the scdf file.
#' @param decreasing If you expect data to be lower in the B phase, set
#'   `decreasing = TRUE`. Default is `decreasing = FALSE`.
#' @param phases A vector of two characters or numbers indicating the two phases
#'   that should be compared. E.g., `phases = c("A","C")` or `phases = c(2,4)`
#'   for comparing the second to the fourth phase. Phases could be combined by
#'   providing a list with two elements. E.g., `phases = list(A = c(1,3), B =
#'   c(2,4))` will compare phases 1 and 3 (as A) against 2 and 4 (as B). Default
#'   is `phases = c(1,2)`.
#' @param model Model used for calculating the dummy parameters (see Huitema &
#'   McKean, 2000). Default is `model = "W"`. Possible values are: `"B&L-B"`,
#'   `"H-M"`, `"W"`, and deprecated `"JW"`.
#' @param contrast Sets contrast_level and contrast_slope. Either "first",
#'   "preceding" or a contrast matrix.
#' @param contrast_level Either "first", "preceding" or a contrast matrix. If NA
#'   contrast_level is a copy of contrast.
#' @param contrast_slope Either "first", "preceding" or a contrast matrix. If NA
#'   contrast_level is a copy of contrast.
#' @param trend A logical indicating if a trend parameters is included in the
#'   model.
#' @param level A logical indicating if a level parameters is included in the
#'   model.
#' @param slope A logical indicating if a slope parameters is included in the
#'   model.
#' @param nice If set TRUE (default) output values are rounded and optimized for
#'  publication tables.
#' @param ... Further arguments passed to the function.
#' @keywords internal
.inheritParams <- function(data, scdf, dvar, mvar, pvar, decreasing, 
                           phases, model, contrast, contrast_level, 
                           contrast_slope, trend, level, slope, nice,
                           ...) {
  
} 