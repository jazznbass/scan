#' @rdname transform.scdf
#' @param lag Number of values surrounding a value to calculate the average
#' @export
moving_median <- function(x, lag = 1) .moving_average(x, lag, median)

#' @rdname transform.scdf
#' @param lag Number of values surrounding a value to calculate the average
#' @export

moving_mean <- function(x, lag = 1) .moving_average(x, lag, mean)

#' @rdname transform.scdf
#' @param f the proportion of surrounding data influencing each data point.
#' @param mt A vector with measurement times.
#' @export
local_regression <- function(x, mt = 1:length(x), f = 0.2) {
  lowess(x ~ mt, f = f)$y
}

#' @export
#' @rdname transform.scdf
#' @param x A vector
#' @param first_of A logical vector
#' @param positions  A numeric vector with relative positions to the first 
#'  appearance of a TRUE value in x.
set_na_at <- function(x, first_of, positions = 0) {
  x[match(TRUE, first_of) + positions] <- NA
  x
}

#' @export
#' @rdname transform.scdf
#' @param x A vector.
#' @param at A logical vector. E.g. `phase == "A"`. The first TRUE value of that
#'   vector is the target position for centring. By default, this is the first
#'   position.
#' @param shift A value indicating a shift in measurement times for centring.
#'   E.g. `shift = 4` will centre four measurement-times after the position
#'   defined by the `at` and `part` arguments.
#' @param part A numeric value between 0 and 1. `0` refers to the first `TRUE`
#'   in the `at` vector, `1` to the last, and `0.5` to the midpoint of the
#'   sequence of `TRUE` values. E.g. if you want to centre at the middle of
#'   phase A, set `at = phase == A, part = 0.5`. Note: decimals are rounded to
#'   integers.

center_at <- function(x, at = TRUE, shift = 0, part = 0) {
  x - x[match(TRUE, at) + shift + round(sum(at, na.rm = TRUE) * part)]
}


#' @export
#' @rdname transform.scdf
#' @param x A logical vector.
#' @param positions A numeric vector with relative positions to the first 
#'  appearance of a TRUE value in x.

first_of <- function(x, positions = 0) {
  .deprecated_warning("set_na_at", "first_of")
  match(TRUE, x) + positions
}

#' @export
#' @rdname transform.scdf
across_cases <- function(...) {
  # a helper function
}

#' @export
#' @rdname transform.scdf
all_cases <- function(...) {
  # a helper function
}

#' @export
#' @rdname transform.scdf
rowwise <- function(...) {
  # a helper function
}


