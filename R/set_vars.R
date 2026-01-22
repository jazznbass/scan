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
  if (!missing(dvar)) dv(data) <- dvar
  if (!missing(mvar)) mt(data) <- mvar
  if (!missing(pvar)) phase(data) <- pvar
  
  data
}

#'@rdname set_vars
#'@export
set_dvar <- function(data, dvar) {
  dv(data) <- dvar
  data
}

#'@rdname set_vars
#'@export
set_mvar <- function(data, mvar) {
  mt(data) <- mvar
  data
}

#'@rdname set_vars
#'@export
set_pvar <- function(data, pvar) {
  phase(data) <- pvar
  data
}
