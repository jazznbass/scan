#' Percent exceeding the median (PEM)
#'
#' Percentage of the phase B measurements that lie above the median of phase A.
#'
#' @details With `decreasing = TRUE` the measurements below the median are
#'   counted instead. `FUN` replaces the median by another measure of central
#'   tendency; further arguments for it are passed through `...`, so
#'   `FUN = mean, trim = 0.1` compares against a ten percent trimmed mean. The
#'   function has to take a numeric vector and an `na.rm` argument and return a
#'   single number.
#'
#'   The number of exceeding measurements is tested against chance with a one
#'   sided binomial test at a probability of 0.5. A chi-squared goodness of fit
#'   test against the same distribution is added with `chi.test = TRUE`.
#'
#'   Missing values are dropped. A case without measurements in one of the
#'   phases gives `NA`, and so does a `FUN` that returns `NA`, which is reported
#'   with a warning.
#' @inheritParams .inheritParams
#' @param binom.test If `TRUE`, a binomial test against a 50/50 distribution is
#'   computed.
#' @param chi.test If `TRUE`, a chi-squared test against a 50/50 distribution is
#'   added.
#' @param FUN Function that gives the value of phase A the measurements of phase
#'   B are compared with.
#' @param \dots Further arguments passed to `FUN`.
#' @return An object of class `sc_pem` with the elements:
#'  |  |  |
#'  | --- | --- |
#'  | `PEM` | Data frame with one row per case: `Case`, `PEM`, and the statistics of the tests that were computed. |
#'  | `test` | The test statistics of that data frame as a matrix, with the case names as row names. |
#' @author Juergen Wilbert
#' @family overlap functions
#' @examples
#' pem(exampleAB)
#'
#' # with the chi-squared test
#' pem(exampleAB, chi.test = TRUE)
#'
#' # compared against a ten percent trimmed mean instead of the median
#' pem(exampleAB, FUN = mean, trim = 0.1)
#'
#' # data that are expected to decrease in phase B
#' pem(exampleAB_decreasing, decreasing = TRUE)
#' @order 1
#' @export
pem <- function(data, dvar, pvar, 
                decreasing = FALSE, 
                binom.test = TRUE, 
                chi.test = FALSE, 
                FUN = median, 
                phases = c(1, 2), 
                ...) {

  # set default attributes
  if (missing(dvar)) dvar <- dv(data)
  if (missing(pvar)) pvar <- phase(data)
  dv(data) <- dvar
  phase(data) <- pvar
  
  data <- .prepare_scdf(data)
  data <- recombine_phases(data, phases = phases)$data
  
  N <- length(data)
  
  PEM       <- rep(NA_real_, N)
  chi       <- rep(NA_real_, N)
  chi.df    <- rep(NA_real_, N)
  chi.p     <- rep(NA_real_, N)
  binom.p   <- rep(NA_real_, N)
  positives <- rep(NA_real_, N)
  total     <- rep(NA_real_, N)
  
  
  for(i in 1:N) {
    A <- data[[i]][, dvar][data[[i]][, pvar] == "A"]
    B <- data[[i]][, dvar][data[[i]][, pvar] == "B"]
    A <- A[!is.na(A)]
    B <- B[!is.na(B)]
    if (length(A) == 0L || length(B) == 0L) {
      PEM[i] <- NA_real_
      next
    }
    nB <- length(B)
    exceeding <- if (!decreasing) {
      sum(B > FUN(A, ...))
    } else {
      sum(B < FUN(A, ...))
    }

    if (is.na(exceeding)) {
      warn("Case ", i, ": FUN returned NA. PEM is set to NA.")
      next
    }

    PEM[i] <- exceeding / nB * 100

    if (binom.test) {
      bi <- binom.test(exceeding, nB, alternative = "greater")
      positives[i] <- bi$statistic
      total[i]     <- bi$parameter
      binom.p[i]   <- bi$p.value
    }
    if (chi.test) {
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
  
  PEM <- cbind(Case = revise_names(data), PEM = PEM, as.data.frame(stats.ma))
  row.names(PEM) <- NULL
  out <- list(PEM = PEM, test = stats.ma, decreasing = decreasing)
  class(out) <- c("sc_pem")
  attributes(out)[opts("phase", "dv")] <- list(pvar, dvar)
  out
}
