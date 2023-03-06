#' Set and get scdf attributes
#'
#' @param x Variable
#' @param var Attribute
#'
#' @return Attribute value
#' @keywords internal
#' @export
scdf_attr <- function(x, var = NULL) {
  if (is.null(var)) return(attr(x, opt("scdf")))
  out <- attr(x, opt("scdf"))
  out[[var]]
}

#' @rdname scdf_attr
#' @param value set value
#' @export
"scdf_attr<-" <- function(x, var, value) {
  scdf_attr <- attr(x, opt("scdf"))
  if (is.null(scdf_attr)) scdf_attr <- list()
  
  scdf_attr[[var]] <- value
  attr(x, opt("scdf")) <- scdf_attr
  x
}

#' @rdname scdf_attr
#' @param scdf An [scdf] object.
dv <- function(scdf) scdf_attr(scdf, opt("dv"))
"dv<-" <- function(x, value) {
  scdf_attr(x, opt("dv")) <- value
  x
}


#' @rdname scdf_attr
mt <- function(scdf) scdf_attr(scdf, opt("mt"))
"mt<-" <- function(x, value) {
  scdf_attr(x, opt("mt")) <- value
  x
}

#' @rdname scdf_attr
phase <- function(scdf) scdf_attr(scdf, opt("phase"))
"phase<-" <- function(x, value) {
  scdf_attr(x, opt("phase")) <- value
  x
}