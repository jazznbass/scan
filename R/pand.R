#' Percentage of all non-overlapping data
#'
#' The `pand()` function calculates the percentage of all non-overlapping data
#' (PAND; Parker, Hagan-Burke, & Vannest, 2007), an index to quantify a level
#' increase (or decrease) in performance after the onset of an intervention.
#'
#' The PAND indicates nonoverlap between phase A and B data ([PND()]), but uses
#' all data and is therefore not based on one single (probably unrepresentative)
#' datapoint.  Furthermore, PAND allows the comparison of real and expected
#' associations (Chi-square test) and estimation of the effect size Phi, which
#' equals Pearsons r for dichotomous data.  Thus, phi-Square is the amount of
#' explained variance. The original procedure for computing the PAND (Parker,
#' Hagan-Burke, & Vannest, 2007) does not account for ambivalent datapoints
#' (ties).  The newer `NAP` overcomes this problem and has better
#' precision-power (Parker, Vannest, & Davis, 2014).
#'
#' @inheritParams .inheritParams
#' @param correction The default `correction = TRUE` makes `pand` use a
#'   frequency matrix, which is corrected for ties. A tie is counted as the half
#'   of a measurement in both phases. Set `correction = FALSE` to use the
#'   uncorrected matrix, which is not recommended.
#' @return \item{pand}{Percentage of all non-overlapping data.}
#'   \item{phi}{Effect size Phi based on expected and observed values.}
#' \item{perc_overlap}{Percentage of overlapping data points.} \item{overlaps}{Number of
#' overlapping data points.} \item{n}{Number of data points.} \item{N}{Number
#' of cases.} \item{nA}{Number of data points in phase A.} \item{nB}{Number of
#' data points in phase B.} \item{pA}{Percentage of data points in phase A.}
#' \item{pB}{Percentage of data points in phase B.} \item{matrix}{2x2 frequency
#' matrix of phase A and B comparisons.} \item{matrix_counts}{2x2 counts matrix
#' of phase A and B comparisons.} \item{correlation}{A list of the
#' \code{correlation} values: statistic, parameter, p.value, estimate,
#' null.value, alternative, method, data.name, correction.}
#' \item{correction}{Logical argument from function call (see `Arguments`
#' above).}
#' @author Juergen Wilbert
#' @family overlap functions
#' @references Parker, R. I., Hagan-Burke, S., & Vannest, K. (2007). Percentage
#'   of All Non-Overlapping Data (PAND): An Alternative to PND. *The Journal of
#'   Special Education, 40*, 194-204.
#'
#'   Parker, R. I., & Vannest, K. (2009). An Improved Effect Size for
#'   Single-Case Research: Nonoverlap of All Pairs. *Behavior Therapy, 40*,
#'   357-367.
#' @examples
#'
#' ## Calculate the PAND for a MMBD over three cases
#' gunnar <- scdf(c(2,3,1,5,3,4,2,6,4,7), B_start = 5)
#' birgit <- scdf(c(3,3,2,4,7,4,2,1,4,7), B_start = 4)
#' bodo   <- scdf(c(2,3,4,5,3,4,7,6,8,7), B_start = 6)
#' mbd <- c(gunnar, birgit, bodo)
#' pand(mbd)
#' pand(bodo)
#'
#' ## Calculate the PAND with an expected decrease of phase B scores
#' cubs <- scdf(c(20,22,24,17,21,13,10,9,20,9,18), B_start = 5)
#' pand(cubs, decreasing = TRUE)
#'
#' @export
pand <- function(data, dvar, pvar, 
                 decreasing = FALSE, 
                 correction = TRUE, 
                 phases = c(1, 2)) {
  
  # set default attirubtes
  if (missing(dvar)) dvar <- dv(data)
  if (missing(pvar)) pvar <- phase(data)
  
  dv(data) <- dvar
  phase(data) <- pvar
  
  data <- .prepare_scdf(data, na.rm = TRUE)
  data <- recombine_phases(data, phases = phases)$data

  N <- length(data)
  
  A <- lapply(data, function(x) x[x[[pvar]] == "A", dvar])
  B <- lapply(data, function(x) x[x[[pvar]] == "B", dvar])
  
  phase_real <- 
    lapply(
      data, function(x) 
        as.numeric(as.factor(x[[pvar]][sort.list(x[[dvar]])]))
    )
  phase_expected <- lapply(data, function(x) as.numeric(as.factor(x[[pvar]])))
  
  tmp <- getOption("warn")
  options(warn = -1)
  results_cor <- cor.test(
    unlist(phase_real), unlist(phase_expected), method = "kendall"
  )
  options(warn = tmp)
  
  nA <- sum(sapply(A, length))
  nB <- sum(sapply(B, length))
  n <- nA + nB
  
  overlaps_cases <- rep(NA, N)
  overlaps_A <- 0
  overlaps_B <- 0
  
  for (i in 1:N) {
    z <- data[[i]][, dvar, drop = FALSE]
    n1 <- length(A[[i]])
    n2 <- length(B[[i]])
    n12 <- n1 + n2
    
    rang <- sort.list(unlist(z), decreasing = decreasing)
    AB <- sum(rang[1:n1] > n1)
    BA <- sum(rang[(n1 + 1):n12] <= n1)
    if(correction) {
      ord <- z[[1]][rang]
      AB <- AB + 0.5 * sum(ord[1:n1] == min(ord[(n1 + 1):n12]))
      BA <- BA + 0.5 * sum(ord[(n1 + 1):n12] == max(ord[1:n1]))
    }
    overlaps_cases[i] <- AB + BA
    overlaps_A <- overlaps_A + AB
    overlaps_B <- overlaps_B + BA
  }
  
  overlaps <- sum(overlaps_cases)
  perc_overlap <- overlaps / n * 100
  pA <- nA / n
  pB <- nB / n
  
  b <- overlaps_A / n
  c <- overlaps_B / n
  a <- pA - b
  d <- pB - c
  phi <- (a / (a + c)) - (b / (b + d))
  pand <- 100 - perc_overlap
  mat <- matrix(c(a, b, c, d), nrow = 2)
  mat2 <- mat * n
  
  out <- list(
    pand = pand, 
    phi = phi, 
    perc_overlap = perc_overlap, 
    overlaps_cases = overlaps_cases, 
    overlaps = overlaps, 
    n = n, 
    N = N, 
    nA = nA, 
    nB = nB, 
    pA = pA, 
    pB = pB, 
    matrix = mat, 
    matrix_counts = mat2, 
    correlation = results_cor, 
    correction = correction
  )
  
  class(out) <- c("sc_pand")
  attr(out, opt("phase")) <- pvar
  attr(out, opt("dv")) <- dvar
  out
}

