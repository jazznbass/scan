#' Transform variables in every single case of a single case data frame
#'
#' Takes a scdf and applies transformations to each individual case. This is
#' useful to calculate or modify new variables.
#'
#' This function is a method of the generic transform function. Unlike the
#' generic function, it calculates expressions serially. This means that the
#' results of the calculation of one expression are the basis for the following
#' computations. The 'all' function is a helper function that calculates values
#' across all cases. It also takes an expression as an argument. For example,
#' \code{mean(all(values))} calculates the mean of the values across all cases.
#' \code{mean(all(values[phase == "A"]))} will calculate the mean of all values
#' where phase is A.
#'
#' @param _data A scdf.
#' @param ... Expressions.
#' @return A scdf.
#' @export
#' @examples
#' ## Creates a single-case with frequency distributions. The proportion and
#' ## percentage of the frequencies are calculated with transform:
#' design <- design(
#'  n = 3,
#'  level = 5,
#'  distribution = "binomial",
#'  n_trials = 20,
#'  start_value = 0.5
#' )
#' study <- random_scdf(design)
#' transform(study, proportion = values/trials, percentage = proportion * 100)
#'
#' ## Z standardize the dependent variable and add two new variables:
#' exampleAB %>%
#'   transform(
#'     values = scale(values),
#'     mean_values = mean(values),
#'     sd_values = sd(values)
#'   )
#'
#' ## Use `all` to calculate global variables.
#' exampleAB %>%
#'   transform(
#'     values_center_case = values - mean(values[phase == "A"]),
#'     values_center_global = values - mean(all(values[phase == "A"])),
#'     value_dif = values_center_case - values_center_global
#'   )
#'
#' ## Standardize the values by the overall mean and standarddeviation
#' exampleABC %>%
#'   transform(values = (values - mean(all(values))) / sd(all(values)))
#'
#' ## Three helper functions to smooth the data
#' Huber2014$Berta %>%
#' transform(
#'   "compliance (moving median)" = moving_median(compliance),
#'   "compliance (moving mean)" = moving_mean(compliance),
#'   "compliance (local regression)" = local_regression(compliance, mt)
#' )
#'
#' ## Function invalidate() helps to set NAs for specific phases.
#' ## E.g., you want to delete the first two values of phase A and the first
#' ## value of phase B and its preceding value.
#'
#' byHeart2011 %>%
#'   transform(
#'     values = invalidate(values, phase == "A", 0:1),
#'     values = invalidate(values, phase == "B", -1:0)
#'   )
transform.scdf <- function(`_data`, ...) {
  f <- substitute(list(...))
  df_all <- as.data.frame(`_data`)
  
  for(i in seq_along(`_data`)) {
    .list <- as.list(`_data`[[i]])
    .list$all_cases <- .list$all <- function(x) {
      x <- substitute(x)
      eval(x, df_all)
    }
    
    for(j in 2:length(f)) {
      new <- eval(f[c(1,j)], .list, parent.frame())
      `_data`[[i]][[names(new)]] <- new[[1]]
      .list[[names(new)]] <- new[[1]]
      
    }
  }
  `_data`
}

#' @export
#' @rdname transform.scdf
#' @param x A vector.
#' @param first_of A logical vector. The first appearance of a TRUE value is the 
#'   reference position.
#' @param positions A numeric vector with relative positions to the first 
#'   appearance of a TRUE value in filter.
invalidate <- function(x, first_of, positions) {
  x[match(TRUE, first_of) + positions] <- NA
  x
}

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
local_regression <- function(x, f = 0.2, mt = 1:length(x)) {
  lowess(x ~ mt, f = f)$y
}

