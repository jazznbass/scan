#' Percent exceeding the median
#' 
#' The \code{pem} function returns the percentage of phase B data exceeding the
#' phase A median.  Additionally, a chi square test against a 50/50
#' distribution is computed.  Different measures of central tendency can be
#' addressed for alternative analyses.
#' 
#' @inheritParams .inheritParams
#' @param binom.test Computes a binomial test for a 50/50 distribution. Default
#' is \code{binom.test = TRUE}.
#' @param chi.test Computes a Chi-square test. The default setting
#' \code{chi.test = FALSE} skips the Chi-square test.
#' @param FUN Data points are compared with the phase A median. Use this
#' argument to implement alternative measures of central tendency. Default is
#' \code{FUN = median}
#' @param \dots Additional arguments for the \code{FUN} parameter (e.g.
#' \code{FUN = mean, trim = 0.1} will use the 10 percent trimmed arithmetic
#' mean instead of the median for comparisons). The function must take a vector
#' of numeric values and the \code{na.rm} argument and return a numeric value.
#' @author Juergen Wilbert
#' @family overlap functions
#' @examples
#' 
#' ## Calculate the PEM including the Binomial and Chi-square tests for a single-case
#' dat <- rSC(5, level = 0.5)
#' pem(dat, chi.test = TRUE)
#' 
#' @export
pem <- function(data, dvar, pvar, decreasing = FALSE, binom.test = TRUE, chi.test = FALSE, FUN = median, phases = c(1, 2), ...) {

  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- scdf_attr(data, .opt$dv) else scdf_attr(data, .opt$dv) <- dvar
  if (missing(pvar)) pvar <- scdf_attr(data, .opt$phase) else scdf_attr(data, .opt$phase) <- pvar
 
  data <- .SCprepareData(data, na.rm = TRUE)
  data <- .keepphasesSC(data, phases = phases, pvar = pvar)$data
  
  N <- length(data)
  
  PEM       <- rep(NA, N)
  chi       <- rep(NA, N)
  chi.df    <- rep(NA, N)
  chi.p     <- rep(NA, N)
  binom.p   <- rep(NA, N)
  positives <- rep(NA, N)
  total     <- rep(NA, N)
  
  
  for(i in 1:N) {
    A <- data[[i]][, dvar][data[[i]][, pvar] == "A"]
    B <- data[[i]][, dvar][data[[i]][, pvar] == "B"]
    if (!decreasing)
      PEM[i] <- mean(B > FUN(A,...)) * 100
    if (decreasing)
      PEM[i] <- mean(B < FUN(A,...)) * 100
    if(binom.test) {
      nB <- length(B)
      bi <- binom.test(round(PEM[i] / 100  * nB), nB, alternative = "greater")
      positives[i] <- bi$statistic
      total[i]     <- bi$parameter
      binom.p[i]   <- bi$p.value
    }
    if(chi.test) {
      nB <- length(B)
      exceeding <- PEM[i] / 100  * nB
      res <- chisq.test(c(exceeding, nB - exceeding), p = c(0.5, 0.5))
      chi[i]    <- res$statistic
      chi.df[i] <- res$parameter
      chi.p[i]  <- res$p.value
    }
  }
  stats.ma <- cbind(positives, total,binom.p)
  colnames(stats.ma) <- c("positives","total","binom.p")
  rownames(stats.ma) <- names(data)
  if(chi.test) {
    cn <- c(colnames(stats.ma),"Chi", "DF", "p")
    stats.ma <- cbind(stats.ma, chi, chi.df, chi.p)
    colnames(stats.ma) <- cn
  }
  
  out <- list(PEM = PEM, test = stats.ma, decreasing = decreasing)
  class(out) <- c("sc","PEM")
  out
}
