#' Set analysis variables in an scdf object
#' 
#' This function allows to set or change the dependent variable,
#' measurement-time variable, and phase variable in an scdf object.
#'
#' @inheritParams .inheritParams
#' @param dvar Character string. Name of the dependent variable.
#' @param mvar Character string. Name of the measurement-time variable.
#' @param pvar Character string. Name of the phase variable.
#' @family data manipulation functions
#' @return An \code{scdf} object with updated variable settings.
#' @author Juergen Wilbert
#' @examples 
#' exampleAB_add |>
#'   set_dvar("depression") |>
#'   describe()
#' @export
set_vars <- function(data, dvar, mvar, pvar) {
  if (!missing(dvar)) data <- set_dvar(data, dvar)
  if (!missing(mvar)) data <- set_mvar(data, mvar)
  if (!missing(pvar)) data <- set_pvar(data, pvar)
  
  data
}

.check_var <- function(data, var, arg) {
  check_args(by_class(data, "scdf"))
  if (!is.character(var) || length(var) != 1) {
    abort("Argument ", arg, " must be a single variable name.")
  }
  if (!all(vapply(data, function(case) var %in% names(case), logical(1)))) {
    abort("Variable '", var, "' is not part of every case of the scdf.")
  }
}

#'@rdname set_vars
#'@export
set_dvar <- function(data, dvar) {
  .check_var(data, dvar, "dvar")
  dv(data) <- dvar
  data
}

#'@rdname set_vars
#'@export
set_mvar <- function(data, mvar) {
  .check_var(data, mvar, "mvar")
  mt(data) <- mvar
  data
}

#'@rdname set_vars
#'@export
set_pvar <- function(data, pvar) {
  .check_var(data, pvar, "pvar")
  phase(data) <- pvar
  data
}
