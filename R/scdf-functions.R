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


checkSCDF <- function(data) {
  cat("Checking object ...\n\n")
  if (!identical(class(data),c("scdf","list")))
    cat("Object is not of class 'scdf'.\n")
  if (!("list" %in% class(data))) {
    cat("Object is not of class 'list'.\n")
    return(invisible(FALSE))
  }
  if (!all(unlist(lapply(data, function(x) is.data.frame(x))))) {
    cat("Not all list-elements are data frames.\n")
    return(invisible(FALSE))
  }
  if (!all(unlist(lapply(data, function(x) {c("phase") %in% names(x)})))) {
    cat("Not all dataframes have a phase column.\n")
    return(invisible(FALSE))
  }
  if (!all(unlist(lapply(data, function(x) {c("values") %in% names(x)})))) {
    cat("Not all dataframes have a values column.\n")
    return(invisible(FALSE))
  }
  if (!all(unlist(lapply(data, function(x) {c("mt") %in% names(x)}))))
    cat("Note: Not all dataframes have an 'mt' column.\n")
  phases <- rle(as.character(data[[1]]$phase))$values
  if (!all(unlist(lapply(data, function(x) identical(rle(as.character(x$phase))$values, phases)))))
    cat("Warning: Phases are not identical for all cases.\n")
  cat("Done!\n")
  return(invisible(FALSE))
}

#' @rdname scdf
#' @param MT Deprecated: Measurement times
#' @export
makeSCDF <- function (data, B.start = NULL, MT = NULL){
  warning("This function is deprecated. Please use the scdf function.\n\n")
  args <- list(
    values = data,
    B.start = B.start
  )
  if (!is.null(MT)) args$mt <- MT
    
  do.call(scdf, args)
}

#' scdf objects
#' Tests for objects of type "scdf"
#'
#' @param x An object to be tested
#' @return Returns TRUE or FALSE depending on whether its argument is of scdf type or not.
#' @export
is.scdf <- function(x) inherits(x, "scdf")

