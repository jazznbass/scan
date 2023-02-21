#' Show scdf cases in viewer
#' 
#' Each case with in scdf opens a separate viewer tab.
#'
#' @param scdf A scdf object
#' @param max Maximum number of cases opened
#'
#' @return
#' @export
#'
#' @examples
#' ## show(exampleABC)
show <- function(scdf, max = 5) {
  if (length(scdf) > max) {
    cat("To many cases to view. Set argument 'max' to a higher value.")
    return()
  }
  
  var_dv <- scdf_attr(scdf, .opt$dv)
  var_phase <- scdf_attr(scdf, .opt$phase)
  var_mt <- scdf_attr(scdf, .opt$mt)
  
  mapply(function(x, y) {
      attr(x[[var_dv]], "label") <- "Dependent variable"
      attr(x[[var_mt]], "label") <- "Measurement time"
      attr(x[[var_phase]], "label") <- "Phase"
      utils::View(x, y)
    }, 
    scdf, names(scdf)
  )
  
}
