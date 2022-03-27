#' Shift values in a single-case data file
#'
#' *This function has been replaced by the much more versatile `transform`
#' function.*
#' Shifting the values might be helpful in cases where the measurement time
#' is given as a time variable (see example below).
#'
#' @inheritParams .inheritParams
#' @param value Number by which to shift the values
#' @param var Character string with the name of the target variable.
#' Defaults to the measurement time variable.
#'
#' @return A scdf with shifted data
#' @family data manipulation functions
#' @export
#'
#' @examples
#' ### Shift the measurement time for a better estimation of the intercept
#' ex <- shift(example_A24, value = -1996)
#' plm(ex)
#' 
#' # Please use transform instead:
#' ex <- transform(example_A24, year = year - 1996)
#' plm(ex)
shift <- function(data, value, var) {
  .deprecated_warning("transform", "shift")
  if (missing(var)) var <- scdf_attr(data, .opt$mt)
  for (i in 1:length(data)) {
    data[[i]][[var]] <- data[[i]][[var]] + value
  }
  data
}
