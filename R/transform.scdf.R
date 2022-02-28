#' Transform variables in every single case of a single case data frame
#'
#' Takes a scdf and applies transformations to each individual case. This is
#' useful to calculate or modify new variables.
#'
#' This function is a method of the generic transformation function. Unlike this
#' one, expressions are calculated serially. This means that the results of the
#' calculation of an expression are the basis for the following calculations.
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
#' ## Z standardize the dependent variable:
#' transform(exampleAB, values = scale(values))

transform.scdf <- function(`_data`, ...) {
  f <- substitute(list(...))
  for(i in seq_along(`_data`)) {
    for(j in 2:length(f)) {
      new <- eval(f[c(1,j)], `_data`[[i]], parent.frame())
      `_data`[[i]][[names(new)]] <- new[[1]]
    }
  }
  `_data`
}
