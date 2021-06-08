#' Validity check for an scdf object
#'
#' @param object An scdf object
#'
#' @return TRUE or Error messages.
#' @export

check_scdf <- function(object) {
  results <- .check_scdf(object)
  if (isTRUE(results)) {
    message("No errors or warnings.")
    return(invisible(TRUE))
  }
  if(length(results$warnings) > 0) {
    warning(results$warnings)
  }
  
  if(length(results$errors) > 0) {
    stop(results$errors)
  }
}

.check_scdf <- function(object) {
  
  errors <- character()
  warnings <- character()
  
  # check class
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
  
  # check all elements are data frames
  if (!all(unlist(lapply(object, function(x) is.data.frame(x))))) {
    msg <- "Not all list-elements are data frames.\n"
    errors <- c(errors, msg)
  }
  
  # check all cases have phase var
  if (!all(unlist(lapply(object, function(x) {var_phase %in% names(x)})))) {
    msg <-paste0(
      "scdf has phase variable ",
      "'", var_phase, "' ",
      "but not all cases have a variable named ",
      "'", var_phase, "'."
    )
    errors <- c(errors, msg)
  }
  
  # check all cases have mt var
  if (!all(unlist(lapply(object, function(x) {var_mt %in% names(x)})))) {
    msg <-paste0(
      "scdf has measurement-time variable ",
      "'", var_mt, "' ",
      "but not all cases have a variable named ",
      "'", var_mt, "'."
    )
    errors <- c(errors, msg)
  }
  
  # check all cases have dv var
  if (!all(unlist(lapply(object, function(x) {var_dv %in% names(x)})))) {
    msg <-paste0(
      "scdf has dependent variable ",
      "'", var_dv, "' ",
      "but not all cases have a variable named ",
      "'", var_dv, "'."
    )
    errors <- c(errors, msg)
  }
  
  # check phase design is identical
  phases_1 <- rle(as.character(object[[1]][[var_phase]]))$values
  .ident_phases <- lapply(object, 
    function(x) identical(rle(as.character(x[[var_phase]]))$values, phases_1)
  )
  .ident_phases <- unlist(.ident_phases)
  if (!all(.ident_phases)) {
    msg <- "Phase design is not identical for all cases."
    warnings <- c(warnings, msg)
  }
  
  
  if (length(errors) == 0 && length(warnings) == 0) return(TRUE)
  
  if (length(errors) > 0) errors <- paste(errors, collapse = "\n  ")
  if (length(warnings) > 0) warnings <- paste(warnings, collapse = "\n  ")
  
  return(list(errors = errors, warnings = warnings))

}



