#' Set analysis variables in an scdf
#'
#' @inheritParams .inheritParams
#' @param dvar Character string. Name of the dependent variable.
#' @param mvar Character string. Name of the measurement-time variable.
#' @param pvar Character string. Name of the phase variable.
#' @family data manipulation functions
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
