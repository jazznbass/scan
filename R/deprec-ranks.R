#' (Deprecated) Rank-transformation of single-case data files
#'
#' *This function is superseded by the more versatile 
#' \code{\link{transform.scdf}} function.*
#'
#' @inheritParams .inheritParams
#' @param var A string or string vector with the names of the variables to be
#'   ranked.
#' @param grand If TRUE, ranks will be calculated across all cases. If FALSE
#'   ranks are calculated within each case.
#' @param ... Additional parameters passed to the \code{\link{rank}} function.
#' @return An \code{scdf} object where the values of the variable(s) are
#'   replaced with ranks.
#' @author Juergen Wilbert
#' @family data manipulation functions
#' @examples
#' # The ranks function is deprecated. Please use transform:
#' res1 <- ranks(Huber2014, var = "compliance")
#' res2 <- transform(Huber2014, across_cases(compliance = rank(compliance, na.last="keep")))
#' identical(res1, res2)
#' 
#' res1 <- ranks(Huber2014, var = "compliance", grand = FALSE)
#' res2 <- transform(Huber2014, compliance = rank(compliance, na.last="keep"))
#' identical(res1, res2)
#' @export
#' @keywords internal

ranks <- function(data, var, grand = TRUE, ...) {
  
  if(identical(grand, FALSE)) {
    for(i in 1:length(data)) {
      for(v in var)
        data[[i]][, v] <- rank(data[[i]][, v], na.last = "keep", ...)
    }
  }

  if(isTRUE(grand)) {
    ATTRIBUTES <- attributes(data)
    data       <- as.data.frame(data)
    data$case  <- factor(data$case, levels = unique(data$case))
    for (v in var) data[, v] <- rank(data[, v], na.last = "keep", ...)
    data <- split(data, data$case)
    data <- lapply(data, function(x) x[, -1])
    attributes(data) <- ATTRIBUTES
  }
    
  data
}
