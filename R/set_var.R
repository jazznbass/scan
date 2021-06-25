#' Set analysis variables in an scdf
#'
#' @inheritParams .inheritParams
#' @param dv Character string. Name of the dependent variable.
#' @param phase Character string. Name of the phase variable.
#' @param mt Character string. Name of the measurement-time variable.
#' @export

set_vars <- function(data, dv, mt, phase) {
  if (!missing(dv)) scdf_attr(data, .opt$dv) <- dv
  if (!missing(mt)) scdf_attr(data, .opt$mt) <- mt
  if (!missing(phase)) scdf_attr(data, .opt$phase) <- phase
  data
}

