#' Rank-transformation of single-case data files
#' 
#' @inheritParams .inheritParams
#' @param var A string or string vector with the names of the variables to be ranked.
#' @param grand If TRUE, ranks will be calculated across all cases. If FALSE ranks are calculated within each case.
#' @param ... Additional paramters passed to the \code{\link{rank}} function.
#' @return An \code{scdf} object where the values of the variable(s) are replaced with ranks.
#' @author Juergen Wilbert
#' @family data manipulation functions
#' @examples
#' Huber2014_rank <- rankSC(Huber2014, var = "compliance")
#' plot(Huber2014_rank, style = "grid2")
#' @export

rankSC <- function(data, var, grand = TRUE, ...) {
  
  if(isFALSE(grand)) {
    for(i in 1:length(data)) {
      for(v in var)
        data[[i]][, v] <- rank(data[[i]][, v], na.last = "keep", ...)
    }
  }

  if(isTRUE(grand)) {
    ATTRIBUTES <- attributes(data)
    data       <- longSCDF(data)
    data$case  <- factor(data$case, levels = unique(data$case))
    for (v in var) data[, v] <- rank(data[, v], na.last = "keep", ...)
    data <- split(data, data$case)
    data <- lapply(data, function(x) x[, -1])
    attributes(data) <- ATTRIBUTES
  }
    
  data
}

