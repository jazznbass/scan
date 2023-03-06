#' Nonoverlap of all Pairs
#'
#' The [nap()] function calculates the nonoverlap of all pairs (NAP; Parker &
#' Vannest, 2009).  NAP summarizes the overlap between all pairs of phase A and
#' phase B data points.  If an increase of phase B scores is expected, a
#' non-overlapping pair has a higher phase B data point.  The NAP equals
#' *number of pairs showing no overlap / number of pairs*.  Because NAP can
#' only take values between 50 and 100 percent, a rescaled and therefore more
#' intuitive NAP (0-100\%) is also displayed.
#'
#' @inheritParams .inheritParams
#' @return \item{nap}{A data frame with NAP and additional values for each
#'   case.} \item{N}{Number of cases.}
#' @author Juergen Wilbert
#' @family overlap functions
#' @references Parker, R. I., & Vannest, K. (2009). An improved effect size for
#'   single-case research: Nonoverlap of all pairs. *Behavior Therapy*, *40*,
#'   357-367.
#' @examples
#'
#' ## Calculate NAP for a study with  lower expected phase B scores
#' ## (e.g. aggressive behavior)
#' gretchen <- scdf(c(A = 12, 14, 9, 10, B = 10, 6, 4, 5, 3, 4))
#' nap(gretchen, decreasing = TRUE)
#'
#' ## Request NAP for all cases from the Grosche2011 scdf
#' nap(Grosche2011)
#'
#' @export

nap <- function(data, dvar, pvar,
                decreasing = FALSE,
                phases = c(1, 2)) {
  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- dv(data) else dv(data) <- dvar
  if (missing(pvar)) pvar <- phase(data) else phase(data) <- pvar
  
  data <- .prepare_scdf(data, na.rm = TRUE)
  data <- recombine_phases(data, phases = phases)$data
  
  casenames <- revise_names(data)
  
  .nap <- function(data) {
    
    values <- split(data[[dvar]], data[[pvar]])
    pairs <- length(values$A) * length(values$B)
    
    if (!decreasing) {
      pos <- pairs -
        sum(unlist(lapply(values$A, function(x) x >= values$B)))
    }
    if (decreasing) {
      pos <- pairs -
        sum(unlist(lapply(values$A, function(x) x <= values$B)))
    }
    
    ties <- sum(unlist(lapply(values$A, function(x) x == values$B)))
    nap <- (pos + (0.5 * ties)) / pairs
    
    test <- wilcox.test(values$A, values$B,
                        alternative = if (decreasing) "greater" else "less",
                        exact = FALSE
    )
    
    list(
      NAP = nap * 100,
      Rescaled = 2 * (nap * 100) - 100,
      Pairs = pairs,
      Positives = pos,
      Ties = ties,
      w = test$statistic,
      p = test$p.value
    )
    
  }  
  
  x <- lapply(data, .nap)
  nap <- do.call(rbind, x)
  
  out <- list(nap = nap)
  class(out) <- c("sc_nap")
  attr(out, opt("phase")) <- pvar
  attr(out, opt("dv")) <- dvar
  out
}
