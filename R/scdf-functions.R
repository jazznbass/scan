#' Combine single-case data frames
#'
#' @param ... scdf objects
#' @param dvar Character string. Name of the dependent variable. Defaults to the dependent variable of the first case provided.
#' @param pvar Character string. Name of the phase variable. Defaults to the phase variable of the first case provided.
#' @param mvar Character string. Name of the measurement-time variable. Defaults to the measurement-time variable of the first case provided.
#'
#' @return A scdf. If not set differently, the attributes of this scdf are copied from the first scdf provided (i.e the first argument of the function). 
#' 
#' @export
combine <- function(..., dvar = NULL, pvar = NULL, mvar = NULL) {
  scdfs <- list(...)
  
  source_attr <- attributes(scdfs[[1]])
  
  case_names <- unlist(lapply(scdfs, names))
   
  data <- unlist(scdfs, recursive = FALSE)
  
  #attributes(data) <- .default_attributes()

  #if (!is.null(source_attr[[.opt$scdf]])) attr(data, .opt$scdf) <- source_attr[[.opt$scdf]]
  attributes(data) <- source_attr
  #attr(data, .opt$scdf) <- source_attr[[.opt$scdf]]
  
  if (!is.null(dvar)) scdf_attr(data, .opt$dv) <- dvar
  if (!is.null(mvar)) scdf_attr(data, .opt$mt) <- mvar
  if (!is.null(pvar)) scdf_attr(data, .opt$phase) <- pvar
  
  
  names(data) <- case_names
  if (!is.null(names(scdfs))) {
    .names <- names(scdfs)[which(names(scdfs) != "")]
    names(data)[which(names(scdfs) != "")] <- .names
    
  }
  
  if (!is.null(dvar)) source_attr[[.opt$dv]] <- dvar
  if (!is.null(mvar)) source_attr[[.opt$mt]] <- mvar
  if (!is.null(pvar)) source_attr[[.opt$phase]] <- pvar
  
  # check class scdf validity
  if (.opt$rigorous_class_check) {
    results <- .check_scdf(data)
    if (!isTRUE(results)) {
      if(length(results$warnings) > 0) {
        warning(results$warnings)
      }
      if(length(results$errors) > 0) {
        stop(results$errors)
      }
    } 
  }
  
  data
}

##' @rdname combine
##' @export
c.scdf <- function(...) {
  combine(...)
}

#' Select an scdf
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



#' scdf objects
#' Tests for objects of type "scdf"
#'
#' @param x An object to be tested
#' @return Returns TRUE or FALSE depending on whether its argument is of scdf type or not.
#' @export
is.scdf <- function(x) inherits(x, "scdf")

