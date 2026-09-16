#' Rescales values of an scdf
#'
#' This function scales the measured values of an scdf file. It allows for mean
#' centering and standardization across all cases included in an scdf.
#'
#' @inheritParams .inheritParams
#' @param ... Names of variables to be standardized, either as object names or
#'  as characters. If none are given, all numeric variables are standardized.
#' @param m The target mean. If set NULL, it is not changed.
#' @param sd The target standard deviation. If set NULL, it is not changed.
#' @return An scdf with the scaled values.
#' @author Juergen Wilbert
#' @family data manipulation functions
#' @keywords manip
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
  
  vars <- as.list(substitute(list(...)))[-1]
  nl <- as.list(names(data[[1]]))
  names(nl) <- names(data[[1]])
  env <- parent.frame()
  vars <- unlist(lapply(
    vars, function(x) eval(x, envir = nl, enclos = env)
  ))
  
  if (length(vars) == 0) {
    vars <- names(data[[1]])[vapply(data[[1]], is.numeric, logical(1))]
    notify("Rescaled ", paste0(vars, collapse = ", "))
  }
  
  for (v in vars) {
    if (!all(vapply(data, function(case) is.numeric(case[[v]]), logical(1))))
      abort("Variable '", v, "' is missing or not numeric in at least one case.")
  }
  
  m_sd <- vapply(vars, function(x) {
      y <- unlist(lapply(data, function(case) case[[x]]))
      c(mean(y, na.rm = TRUE), sd(y, na.rm = TRUE))
    },
    FUN.VALUE = double(2)
  ) 

  for (case in 1:N) {
    for (i in 1:length(vars)) {
      values <- data[[case]][, vars[i]]
      if (!is.null(m) && !is.null(sd)) {
        data[[case]][, vars[i]] <- (values - m_sd[1, i]) / m_sd[2, i] * sd + m
      }
      if (!is.null(m) && is.null(sd)) {
        data[[case]][, vars[i]] <- (values - m_sd[1, i]) + m
      }
      if (is.null(m) && !is.null(sd)) {
        data[[case]][, vars[i]] <- ((values - m_sd[1, i]) / m_sd[2, i] * sd) + m_sd[1, i]
      }
    }
  }

  data
}
