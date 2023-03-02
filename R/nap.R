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
  if (missing(dvar)) dvar <- dv(data)
  if (missing(pvar)) pvar <- phase(data)
  dv(data) <- dvar
  phase(data) <- pvar

  data <- .prepare_scdf(data, na.rm = TRUE)
  data <- recombine_phases(data, phases = phases)$data

  N <- length(data)

  nap <- rep(NA, N)
  pairs <- rep(NA, N)
  pos <- rep(NA, N)
  ties <- rep(NA, N)
  w <- rep(NA, N)
  p <- rep(NA, N)

  for (case in 1:N) {
    values <- split(data[[case]][[dvar]], data[[case]][[pvar]])
    pairs[case] <- length(values$A) * length(values$B)

    if (!decreasing) {
      pos[case] <- pairs[case] -
        sum(unlist(lapply(values$A, function(x) x >= values$B)))
    }
    if (decreasing) {
      pos[case] <- pairs[case] -
        sum(unlist(lapply(values$A, function(x) x <= values$B)))
    }
    
    ties[case] <- sum(unlist(lapply(values$A, function(x) x == values$B)))
    nap[case] <- (pos[case] + (0.5 * ties[case])) / pairs[case]
    
    test <- wilcox.test(
      values$A, values$B,
      alternative = if (decreasing) "greater" else "less",
      exact = FALSE
    )
    w[case] <- test$statistic
    p[case] <- test$p.value
  }

  nap <- data.frame(
    Case = revise_names(data),
    NAP = nap * 100,
    Rescaled = 2 * (nap * 100) - 100,
    Pairs = pairs,
    Positives = pos,
    Ties = ties,
    W = w,
    p = p
  )

  out <- list(nap = nap, N = N)
  class(out) <- c("sc_nap")
  attr(out, opt("phase")) <- pvar
  attr(out, opt("dv")) <- dvar
  out
}
