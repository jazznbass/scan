#' Dummy function to inherit global descriptions of parameters
#'
#' @param data A single-case data frame. See \code{\link{scdf}} to learn about this format.
#' @param dvar Character string with the name of the dependent variable. Defaults to the attributes in the scdf file.
#' @param pvar Character string with the name of the phase variable. Defaults to the attributes in the scdf file.
#' @param mvar Character string with the name of the measurement time variable. Defaults to the attributes in the scdf file.
#' @param decreasing If you expect data to be lower in the B phase, set
#' \code{decreasing = TRUE}. Default is \code{decreasing = FALSE}.
#' @param phases A vector of two characters or numbers indicating the two
#' phases that should be compared. E.g., \code{phases = c("A","C")} or
#' \code{phases = c(2,4)} for comparing the second to the fourth phase. Phases
#' could be combined by providing a list with two elements. E.g., \code{phases
#' = list(A = c(1,3), B = c(2,4))} will compare phases 1 and 3 (as A) against 2
#' and 4 (as B). Default is \code{phases = c("A","B")}.
#' @param model Model used for calculating the slope parameter (see Huitema &
#' McKean, 2000). Default is \code{model = "B&L-B"}. Possible values are:
#' \code{"B&L-B"}, \code{"H-M"}, \code{"Mohr#1"}, \code{"Mohr#2"}, \code{"JW"}, \code{"JW2"}, and
#' \code{"Manly"}.
#' @param trend A logical indicating if a trend parameters is included in the model.
#' @param level A logical indicating if a level parameters is included in the model.
#' @param slope A logical indicating if a slope parameters is included in the model.

.inheritParams <- function(data, dvar, mvar, pvar, decreasing, 
                           phases, model, trend, level, slope) {
  
} 