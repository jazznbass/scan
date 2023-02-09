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
#' @param x A logical vector.
#' @param positions A numeric vector with relative positions to the first 
#'   appearance of a TRUE value in x.
first_of <- function(x, positions = 0) {
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



