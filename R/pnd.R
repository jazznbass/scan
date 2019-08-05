#' Percentage of non-overlapping data
#' 
#' This function returns the percentage of non-overlapping data.  Due to its
#' error-proneness the PND should not be used, but \code{\link{nap}} or
#' \code{\link{pand}} instead (see Parker & Vannest, 2009).
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
pnd <- function(data, dvar, pvar, decreasing = FALSE, phases = c("A","B")) {

  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- scdf_attr(data, .opt$dv) else scdf_attr(data, .opt$dv) <- dvar
  if (missing(pvar)) pvar <- scdf_attr(data, .opt$phase) else scdf_attr(data, .opt$phase) <- pvar
  
  data <- .SCprepareData(data, na.rm = TRUE)
  data <- .keepphasesSC(data, phases = phases, pvar = pvar)$data
  
  PND <- c()
  n.B <- c()
  
  for(i in 1:length(data)) {
    A <- data[[i]][, dvar][data[[i]][, pvar] == "A"]
    B <- data[[i]][, dvar][data[[i]][, pvar] == "B"]
    n.B[i] <- length(B)
    if (!decreasing)
      PND[i] <- sum(B > max(A)) /  n.B[i] * 100
    if (decreasing)
      PND[i] <- sum(B < min(A)) /  n.B[i] * 100
  }
  
  out <- list(PND = PND, case.names = names(data), n.B = n.B)
  class(out) <- c("sc","PND")
  out
}
