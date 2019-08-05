#' Set and get scdf attributes
#'
#' @param x Variable
#' @param var Attribute
#'
#' @return Attribute value
#' @export
scdf_attr <- function(x, var) {
  out <- attr(x, .opt$scdf)
  out[[var]]
}

#' @rdname scdf_attr
#' @param value set value
#' @export
"scdf_attr<-" <- function(x, var, value) {
  scdf_attr <- attr(x, .opt$scdf)
  if (is.null(scdf_attr)) scdf_attr <- list()
  
  scdf_attr[[var]] <- value
  attr(x, .opt$scdf) <- scdf_attr
  x
}

