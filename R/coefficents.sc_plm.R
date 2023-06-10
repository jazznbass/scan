#' Extract coefficients from plm/hplm objects
#'
#' @param object plm or hplm object
#' @param ... not implemented
#'
#' @return data frame with coefficient table
#' @export
#'
#' @examples
#' coefficients(plm(exampleAB$Johanna))
#' 
coef.sc_plm <- function(object, ...) {
  summary(object$full.model)$coef
}

#' @describeIn hplm Extract model coefficients
#' @order 4
#' @inheritParams coef.sc_plm
#' @export
coef.sc_hplm <- function(object, ...) {
  summary(object$hplm)$tTable
}
