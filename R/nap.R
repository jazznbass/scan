#' Nonoverlap of all Pairs
#' 
#' The \code{nap} function calculates the nonoverlap of all pairs (NAP; Parker
#' & Vannest, 2009).  NAP summarizes the overlap between all pairs of phase A
#' and phase B data points.  If an increase of phase B scores is expected, a
#' non-overlapping pair has a higher phase B data point.  The NAP equals
#' \eqn{number of pairs showing no overlap / number of pairs}.  Because NAP can
#' only take values between 50 and 100 percent, a rescaled and therefore more
#' intuitive NAP (0-100\%) is also displayed.
#' 
#' 
#' @inheritParams .inheritParams
#' @return 
#' \item{nap}{A data frame with NAP and additional values for each case.} 
#' \item{N}{Number of cases.} 
#' @author Juergen Wilbert
#' @family overlap functions 
#' @references Parker, R. I., & Vannest, K. (2009). An improved effect size for
#' single-case research: Nonoverlap of all pairs. \emph{Behavior Therapy, 40},
#' 357-367.
#' @examples
#' 
#' ## Calculate NAP for a study with  lower expected phase B scores (e.g. aggressive behavior)
#' gretchen <- scdf(c(A = 12,14,9,10, B = 10,6,4,5,3,4))
#' nap(gretchen, decreasing = TRUE)
#' 
#' ## Request NAP for all cases fom the Grosche2011 data
#' nap(Grosche2011)
#' 
#' @export

nap <- function(data, dvar, pvar, decreasing = FALSE, phases = c(1,2)) {

  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- scdf_attr(data, .opt$dv) else scdf_attr(data, .opt$dv) <- dvar
  if (missing(pvar)) pvar <- scdf_attr(data, .opt$phase) else scdf_attr(data, .opt$phase) <- pvar

  data <- .SCprepareData(data, na.rm = TRUE)
  data <- .keepphasesSC(data, phases = phases, pvar = pvar)$data
  
  N <- length(data)
  
  NAP   <- rep(NA, N)
  PAIRS <- rep(NA, N)
  POS   <- rep(NA, N)
  TIES  <- rep(NA, N)
  W     <- rep(NA, N)
  p     <- rep(NA, N)
  
  for(case in 1:N) {
    df <- data[[case]]
    
    A     <- df[df[, pvar] == "A", dvar]
    B     <- df[df[, pvar] == "B", dvar]
    PAIRS[case] <- length(A) * length(B)
    
    if (!decreasing)
      POS[case] <- PAIRS[case] - sum(sapply(A, function(x) x >= B))
    if (decreasing)
      POS[case] <- PAIRS[case] - sum(sapply(A, function(x) x <= B))
    
    test <- wilcox.test(A, B, alternative = ifelse(decreasing, "greater", "less"), exact = FALSE)
    
    W[case] <- test$statistic
    p[case] <- test$p.value
    TIES[case] <- sum(sapply(A, function(x) x == B))
    NAP[case]  <- (POS[case] + (0.5 * TIES[case])) / PAIRS[case]
    
  }  
  nap <- data.frame(
    Case = names(data), NAP = NAP * 100, Rescaled = 2 * (NAP * 100) - 100, 
    Pairs = PAIRS, Positives = POS, Ties = TIES, W = W, p = p
  )

  out <- list(nap = nap, N = N)
  class(out) <- c("sc","NAP")
  out
}
