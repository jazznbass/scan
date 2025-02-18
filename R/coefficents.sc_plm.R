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
#' @param casewise Returns the estimations for each case
#' @inheritParams coef.sc_plm
#' @export
coef.sc_hplm <- function(object, casewise = FALSE, ...) {
  if (casewise) {
    out <- coef(object$hplm)
    names(out) <- rename_predictors(names(out), object)
    out <- cbind(Case = row.names(out), out)
    row.names(out) <- NULL
    return(out)
  } else {
    summary(object$hplm)$tTable
  }
}
