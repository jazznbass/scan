#' Combine single-case data frames into a multiple case study
#'
#' @param ... scdf objects
#'
#' @return A scdf
#' 
#' @export
combine <- function(...) {
  scdfs <- list(...)
  
  source_attr <- attributes(scdfs[[1]])

  case_names <- unlist(lapply(scdfs, names))
   
  data <- unlist(scdfs, recursive = FALSE)
  attributes(data) <- .defaultAttributesSCDF()

  if (!is.null(source_attr[[.opt$scdf]])) attr(data, .opt$scdf) <- source_attr[[.opt$scdf]]
  
  names(data) <- case_names
  if (!is.null(names(scdfs))) 
    names(data)[which(names(scdfs) != "")] <- names(scdfs)[which(names(scdfs) != "")]
  
  data
}

##' @rdname combine
##' @export
c.scdf <- function(...) {
  combine(...)
}

#' Select a scdf
#'
#' @param x A scdf object
#' @param i A case name from x 
#'
#' @return A scdf
#' @rdname Subsetting
#' @export
`$.scdf`<- function(x, i) {
  if (is.character(i) && !(i %in% names(x))) {
    warning("Unknown case: '", i, "'.")
  }
  out <- x[i]
  attr(out, .opt$scdf) <- attr(x, .opt$scdf)
  class(out) <- c("scdf", "list")
  out
}

##' @rdname Subsetting
##' @export
`[.scdf`<- function(x, i) {
  class(x) <- "list"
  out <- x[i]
  attr(out, .opt$scdf) <- attr(x, .opt$scdf)
  class(out) <- c("scdf", "list")
  out
}


#' as_scdf
#' Converts a data frame to an scdf object
#'
#' @param object A scdf object
#'
#' @export

as_scdf <- function(object) {
  
  if (is.data.frame((object)))
    object <- list(object)

  if (!is.list(object))
    stop("Object must be a data.frame or a list of data.frames.")
  
  attributes(object) <- .defaultAttributesSCDF(attributes(object)) 
  object
  
}

#' scdf objects
#' Tests for objects of type "scdf"
#'
#' @param x An object to be tested
#' @return Returns TRUE or FALSE depending on whether its argument is of scdf type or not.
#' @export
is.scdf <- function(x) inherits(x, "scdf")

