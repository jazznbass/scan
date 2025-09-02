#' Nonoverlap of all Pairs
#'
#' The [nap()] function calculates the nonoverlap of all pairs (NAP; Parker &
#' Vannest, 2009).  NAP summarizes the overlap between all pairs of phase A and
#' phase B data points.  If an increase of phase B scores is expected, a
#' non-overlapping pair has a higher phase B data point. The NAP equals
#' *number of pairs showing no overlap / number of pairs* where ties are
#' counted as half non-overlaps.  Because NAP can take values between 0 and 100
#' percent where values below 50 percent indicate an inverse effect, an nap
#' rescaled from -100 to 100 percent where negative
#' values indicate an inverse effect is also displayed (\eqn{nap_{rescaled} = 2
#' * nap - 100}).
#'
#' @inheritParams .inheritParams
#' @return 
#'  |  |  |
#'  | --- | --- |
#'  | `nap` | A data frame with NAP and additional values for each case. |
#'  | `N` | Number of cases. |
#'  
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
      pos <- sum(unlist(lapply(values$A, function(x) values$B > x)))
    }
    
    if (decreasing) {
      pos <- sum(unlist(lapply(values$A, function(x) values$B < x)))
    }
    
    ties <- sum(unlist(lapply(values$A, function(x) x == values$B)))
    
    non_overlaps <- pos + (0.5 * ties)
    
    nap <- non_overlaps / pairs
    
    test <- wilcox.test(
      values$A, values$B,
      alternative = if (decreasing) "greater" else "less",
      exact = FALSE
    )
    #nap <- (pairs - test$statistic) / pairs
    d <- 3.464 * (1 - sqrt((1 - nap) / 0.5))
    r <- d / sqrt(d^2 + 4)
    data.frame(
      NAP = nap * 100,
      "NAP Rescaled" = 2 * (nap * 100) - 100,
      Pairs = pairs,
      "Non-overlaps" = non_overlaps,
      Positives = pos,
      Ties = ties,
      w = test$statistic,
      p = test$p.value,
      d = d,
      "R\u00B2" = r^2,
      check.names = FALSE
    )
 
  }  
  
  x <- lapply(data, .nap)
  nap <- do.call(rbind, x)
  nap <- cbind(Case = casenames, nap)
  rownames(nap) <- NULL
  
  out <- list(nap = nap)
  class(out) <- c("sc_nap")
  attributes(out)[opts("phase", "dv")] <- list(pvar, dvar)
  out
}
