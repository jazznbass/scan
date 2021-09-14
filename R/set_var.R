#' Set analysis variables in an scdf
#'
#' @inheritParams .inheritParams
#' @param dvar Character string. Name of the dependent variable.
#' @param mvar Character string. Name of the measurement-time variable.
#' @param pvar Character string. Name of the phase variable.
#' @examples 
#' exampleAB_add %>% 
#'   set_dvar("depression") %>%
#'   describe()
#' @export

set_vars <- function(data, dvar, mvar, pvar) {
  if (!missing(dv)) scdf_attr(data, .opt$dv) <- dv
  if (!missing(mt)) scdf_attr(data, .opt$mt) <- mt
  if (!missing(phase)) scdf_attr(data, .opt$phase) <- phase
  data
}

#'@rdname set_vars
#'@export
set_dvar <- function(data, dvar) {
  scdf_attr(data, .opt$dv) <- dvar
  data
}

#'@rdname set_vars
#'@export
set_mvar <- function(data, mvar) {
  scdf_attr(data, .opt$mt) <- mvar
  data
}

#'@rdname set_vars
#'@export
set_pvar <- function(data, pvar) {
  scdf_attr(data, .opt$phase) <- pvar
  data
}
