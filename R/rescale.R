#' Rescales values of an scdf file
#'
#' This function scales the measured values of an scdf file. It allows for mean
#' centering and standardization across all cases included in an scdf.
#'
#' @inheritParams .inheritParams
#' @param ... Names of variables to be standardized.
#' @param m The target mean. If set NULL, it is not changed.
#' @param sd The target standard deviation. If set NULL, it is not changed.
#' @return An scdf with the scaled values.
#' @author Juergen Wilbert
#' @family data manipulation functions
#' @keywords internal
#' @examples
#'
#' ## Standardize a multiple case scdf and compute an hplm
#' exampleAB_50 |>
#'   rescale(values, mt) |>
#'   hplm()
#' @export
rescale <- function(data, 
                    ...,
                    m = 0, 
                    sd = 1) {

  data <- .prepare_scdf(data)

  N <- length(data)
  
  vars <-  list(...) |> substitute() |> sapply(deparse)
  vars <- vars[-1]
  
  if (length(vars) == 0) {
    vars <- sapply(data[[1]], \(x) is.numeric(x)) |> which()
    vars <- names(data[[1]])[vars]
    message(paste0("Rescaled ", paste0(vars, collapse = ", ")))
  }
 
  means <- c()
  sds <- c()
  for (i in 1:length(vars)) {
    means <- c(
      means, 
      mean(unlist(lapply(data, function(x) x[, vars[i]])), na.rm = TRUE)
    )
    sds <- c(
      sds, 
      sd(unlist(lapply(data, function(x) x[, vars[i]])), na.rm = TRUE)
    )
  }

  for (case in 1:N) {
    for (i in 1:length(vars)) {
      values <- data[[case]][, vars[i]]
      if (!is.null(m) && !is.null(sd)) {
        data[[case]][, vars[i]] <- (values - means[i]) / sds[i] * sd + m
      }
      if (!is.null(m) && is.null(sd)) {
        data[[case]][, vars[i]] <- (values - means[i]) + m
      }
      if (is.null(m) && !is.null(sd)) {
        data[[case]][, vars[i]] <- ((values - means[i]) / sds[i] * sd) + means[i]
      }
    }
  }

  data
}
