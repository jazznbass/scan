#' Percentage of all non-overlapping data
#' 
#' The \code{pand} function calculates the percentage of all non-overlapping
#' data (PAND; Parker, Hagan-Burke, & Vannest, 2007), an index to quantify a
#' level increase (or decrease) in performance after the onset of an
#' intervention.
#' 
#' The PAND indicates nonoverlap between phase A and B data (like \code{PND}),
#' but uses all data and is therefore not based on one single (probably
#' unrepresentative) datapoint.  Furthermore, PAND allows the comparison of
#' real and expected associations (Chi-square test) and estimation of the
#' effect size Phi, which equals Pearsons r for dichotomous data.  Thus,
#' phi-Square is the amount of explained variance. The original procedure for
#' computing the PAND (Parker, Hagan-Burke, & Vannest, 2007) does not account
#' for ambivalent datapoints (ties).  The newer \code{NAP} overcomes this
#' problem and has better precision-power (Parker, Vannest, & Davis, 2014).
#' 
#' @inheritParams .inheritParams
#' @param correction The default \code{correction = TRUE} makes \code{pand} use
#' a frequency matrix, which is corrected for ties. A tie is counted as the
#' half of a measurement in both phases. Set \code{correction = FALSE} to use
#' the uncorrected matrix, which is not recommended.
#' @return \item{PAND}{Percentage of all non-overlapping data.}
#' \item{phi}{Effect size Phi based on expected and observed values.}
#' \item{POD}{Percentage of overlapping data points.} \item{OD}{Number of
#' overlapping data points.} \item{n}{Number of data points.} \item{N}{Number
#' of cases.} \item{nA}{Number of data points in phase A.} \item{nB}{Number of
#' data points in phase B.} \item{pA}{Percentage of data points in phase A.}
#' \item{pB}{Percentage of data points in phase B.} \item{matrix}{2x2 frequency
#' matrix of phase A and B comparisons.} \item{matrix.counts}{2x2 counts matrix
#' of phase A and B comparisons.} \item{correlation}{A list of the
#' \code{correlation} values: statistic, parameter, p.value, estimate,
#' null.value, alternative, method, data.name, correction.}
#' \item{correction}{Logical argument from function call (see \code{Arguments}
#' above).}
#' @author Juergen Wilbert
#' @family overlap functions
#' @references Parker, R. I., Hagan-Burke, S., & Vannest, K. (2007). Percentage
#' of All Non-Overlapping Data (PAND): An Alternative to PND. \emph{The Journal
#' of Special Education, 40}, 194-204.
#' 
#' Parker, R. I., & Vannest, K. (2009). An Improved Effect Size for Single-Case
#' Research: Nonoverlap of All Pairs. \emph{Behavior Therapy, 40}, 357-367.
#' @examples
#' 
#' ## Calculate the PAND for a MMBD over three cases
#' gunnar <- scdf(c(2,3,1,5,3,4,2,6,4,7), B.start = 5)
#' birgit <- scdf(c(3,3,2,4,7,4,2,1,4,7), B.start = 4)
#' bodo   <- scdf(c(2,3,4,5,3,4,7,6,8,7), B.start = 6)
#' mbd <- c(gunnar, birgit, bodo)
#' pand(mbd)
#' pand(bodo)
#' 
#' ## Calculate the PAND with an expected decrease of phase B scores
#' cubs <- scdf(c(20,22,24,17,21,13,10,9,20,9,18), B.start = 5)
#' pand(cubs, decreasing = TRUE)
#' 
#' @export
pand <- function(data, dvar, pvar, decreasing = FALSE, correction = TRUE, phases = c("A","B")) {
  
  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- scdf_attr(data, .opt$dv) else scdf_attr(data, .opt$dv) <- dvar
  if (missing(pvar)) pvar <- scdf_attr(data, .opt$phase) else scdf_attr(data, .opt$phase) <- pvar

  data <- .SCprepareData(data, na.rm = TRUE)
  data <- .keepphasesSC(data, phases = phases,pvar = pvar)$data
  
  phase.expected <- list()
  phase.real     <- list()
  A              <- list()
  B              <- list()
  
  N <- length(data)
  
  for (i in 1:N) {
    A[[i]] <- data[[i]][data[[i]][, pvar] == "A", dvar]
    B[[i]] <- data[[i]][data[[i]][, pvar] == "B", dvar]
    data[[i]][, pvar] <- factor(data[[i]][, pvar])
    
    phase.real[[i]]     <- as.numeric(data[[i]][order(data[[i]][, dvar]), pvar])
    phase.expected[[i]] <- as.numeric(data[[i]][, pvar])
    
  }	
  
  tmp <- getOption("warn")
  options(warn = -1)
  results.cor <- cor.test(unlist(phase.real), unlist(phase.expected), method = "kendall")
  options(warn = tmp)
  
  nA <- 0
  nB <- 0
  for (i in 1:N) {
    nA <- nA + length(A[[i]])
    nB <- nB + length(B[[i]])
  }
  
  n <- nA + nB
  
  OD.PP <- rep(NA, N)
  OD.A <- 0
  OD.B <- 0
  
  for (i in 1:N) {
    z <- data[[i]][, dvar, drop = FALSE]
    n1 <- length(A[[i]])
    n2 <- length(B[[i]])
    n12 <- n1 + n2
    
    rang <- order(z, decreasing = decreasing)
    AB <- sum(rang[1:n1] > n1)
    BA <- sum(rang[(n1 + 1):n12] <= n1)
    if(correction) {
      ord <- z[rang, ]
      AB <- AB + 0.5 * sum(ord[1:n1] == min(ord[(n1 + 1):n12]))
      BA <- BA + 0.5 * sum(ord[(n1 + 1):n12] == max(ord[1:n1]))
    }
    OD.PP[i] <- AB + BA
    OD.A <- OD.A + AB
    OD.B <- OD.B + BA
  }
  
  OD <- sum(OD.PP)
  POD <- OD / n * 100
  pA <- nA / n
  pB <- nB / n
  
  b <- OD.A / n
  c <- OD.B / n
  a <- pA - b
  d <- pB - c
  phi <- (a/(a + c))-(b/(b + d))
  PAND <- 100 - POD
  mat <- matrix(c(a,b,c,d), nrow = 2)
  mat2 <- mat * n
  
  out <- list(PAND = PAND, phi = phi, POD = POD, OD.PP = OD.PP, OD = OD, n = n, N = N, nA = nA, nB = nB, pA = pA, pB = pB, matrix = mat, matrix.counts = mat2, correlation = results.cor, correction = correction)
  
  class(out) <- c("sc","PAND")
  out
}

