#' Print methods for scan objects
#'
#' @param x Object 
#' @param digits The minimum number of significant digits to be use. 
#' If set to "auto" (default), values are predefined.
#' @param ... Further parameters passed to the print function
#' @name print.sc
NULL


.note_vars <- function(x) {
  v <- any(attr(x, .opt$dv) != "values")
  p <- attr(x, .opt$phase) != "phase"
  m <- attr(x, .opt$mt) != "mt"
  if (v || p || m) { 
    cat(
      "\nThe following variables were used in this analysis:\n'", 
      paste0(attr(x, .opt$dv), collapse = "/ "), 
      "' as dependent variable, '", 
      paste0(attr(x, .opt$phase), collapse = "/ "), 
      "' as phase variable, and '", 
      paste0(attr(x, .opt$mt), collapse = "/ "), 
      "' as measurement-time variable.\n", 
      sep = ""
    )
  }
}

