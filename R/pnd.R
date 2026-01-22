#' Percentage of non-overlapping data (PND)
#' 
#' This function returns the percentage of non-overlapping data.  Due to its
#' error-proneness the PND should not be used, but \code{\link{nap}} or
#' \code{\link{pand}} instead (see Parker & Vannest, 2009).
#' 
#' PND is calculated by determining the number of data points in phase B that
#' exceed the highest data point in phase A (or are lower than the lowest data
#' point in phase A for decreasing data) divided by the total number of data
#' points in phase B. This value is then multiplied by 100 to get a percentage
#' value.
#' 
#' @inheritParams .inheritParams
#' @param decreasing If you expect data to be lower in the B phase, set
#' \code{decreasing = TRUE}. Default is \code{decreasing = FALSE}.
#' @return \item{PND}{Percentage of non-overlapping data.}
#' @author Juergen Wilbert
#' @family overlap functions
#' @examples
#' 
#' ## Calculate the PND for multiple single-case data
#' pnd(GruenkeWilbert2014)
#' 
#' @export
pnd <- function(data, dvar, pvar, decreasing = FALSE, phases = c(1, 2)) {

  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- dv(data) else dv(data) <- dvar
  if (missing(pvar)) pvar <- phase(data) else phase(data) <- pvar
  
  data <- .prepare_scdf(data, na.rm = TRUE)
  data <- recombine_phases(data, phases = phases)$data
  
  pnd <- c()
  n.B <- c()
  
  for(i in 1:length(data)) {
    A <- data[[i]][, dvar][data[[i]][, pvar] == "A"]
    B <- data[[i]][, dvar][data[[i]][, pvar] == "B"]
    n.B[i] <- length(B)
    if (!decreasing) pnd[i] <- sum(B > max(A)) /  n.B[i] * 100
    if (decreasing) pnd[i] <- sum(B < min(A)) /  n.B[i] * 100
  }
  
  out <- list(PND = pnd, case.names = names(data), n.B = n.B)
  class(out) <- c("sc_pnd")
  attributes(out)[opts("phase", "dv")] <- list(pvar, dvar)
  out
}
