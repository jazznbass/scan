#' Validity check for an scdf object
#'
#' @param object An scdf object
#'
#' @return TRUE or Error messages.
#' @export

check_scdf <- function(object) {
  results <- .check_scdf(object)
  if (isTRUE(results)) TRUE else warning(results)
}

.check_scdf <- function(object) {
  
  errors <- character()
  
  if (!identical(class(object), c("scdf", "list"))) {
    msg <- paste0("Class is not c('scdf', 'list').")
    errors <- c(errors, msg)
  }
  
  scdf_attributes <- attr(object, .opt$scdf)
  
  if (is.null(scdf_attributes)) {
    msg <- paste0("Attribute '", .opt$scdf, "' missing.")
    errors <- c(errors, msg)
    return(errors)
  } 
  
  var_phase <- scdf_attributes[[.opt$phase]]
  var_mt <- scdf_attributes[[.opt$mt]]
  var_dv <- scdf_attributes[[.opt$dv]]
  
  if (!all(unlist(lapply(object, function(x) is.data.frame(x))))) {
    msg <- "Not all list-elements are data frames.\n"
    errors <- c(errors, msg)
  }
  
  if (!all(unlist(lapply(object, function(x) {var_phase %in% names(x)})))) {
    msg <-paste0(
      "Not all dataframes have a phase column named '", var_phase, "'."
    )
    errors <- c(errors, msg)
  }
  
  if (!all(unlist(lapply(object, function(x) {var_mt %in% names(x)})))) {
    msg <- paste0("Not all dataframes have an mt column named '", var_mt, "'.")
    errors <- c(errors, msg)
  }
  
  if (!all(unlist(lapply(object, function(x) {var_dv %in% names(x)})))) {
    msg <-paste0(
      "Not all dataframes have a dependent variable column named '", var_dv, "'."
    )
    errors <- c(errors, msg)
  }
  
  phases <- rle(as.character(object[[1]][[var_phase]]))$values
  
  if (!all(unlist(lapply(object, function(x) identical(rle(as.character(x[[var_phase]]))$values, phases)))))
  {
    msg <- "Phase design is not identical for all cases."
    errors <- c(errors, msg)
  }
  
  
  if (length(errors) == 0) TRUE else c(errors)
}



