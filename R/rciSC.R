#' Reliable change index
#' 
#' \bold{CAUTION! This function is still under development and not ready for
#' use!} The \code{rciSC} function computes three indices of reliable change
#' (Wise, 2004) and corresponding descriptive statistics.
#' 
#' 
#' @aliases rciSC rCi
#' @param data A single-case data frame. See \code{\link{scdf}} to learn about
#' this format.
#' @param dvar Character string with the name of the dependent variable.
#' @param pvar Character string with the name of the phase variable.
#' @param rel Reliability of the measure, used to compute the standard error.
#' Default is \code{rel = 0.8}.
#' @param ci Width of confidence interval as a decimal. Default is \code{ci =
#' 0.95} applying a 95\%-confidence interval.
#' @param graph If set \code{TRUE}, a box plot of phase A and B scores is
#' displayed. \code{graph = FALSE} by default.
#' @param phases A vector of two characters or numbers indicating the two
#' phases that should be compared. E.g., \code{phases = c("A","C")} or
#' \code{phases = c(2,4)} for comparing the second and the fourth phase. Phases
#' could be combined by providing a list with two elements. E.g., \code{phases
#' = list(A = c(1,3), B = c(2,4))} will compare phases 1 and 3 (as A) against 2
#' and 4 (as B). Default is \code{phases = c("A","B")}.
#' @return \item{RCI}{A list of three RCI calculations (Jacobson et al.,
#' Christenden et al., Hageman et al.).} \item{stand.dif}{Standardized
#' difference between mean phase A and B scores.} \item{conf}{A matrix
#' containing the lower and upper confidence interval boundaries of phases A
#' and B.} \item{conf.percent}{Numeric argument from function call (see
#' \code{ci} in \code{Arguments} section).} \item{reliability}{Numeric argument
#' from function call (see \code{Arguments} above).} \item{descriptives}{A
#' matrix containing descriptive statistics for phases A and B: n, mean, SD,
#' SE.} \item{N}{Number of cases.} \item{A}{A vector of phase A scores.}
#' \item{B}{A vector of phase B scores.}
#' @author Juergen Wilbert
#' @references Christensen, L., & Mendoza, J. L. (1986). A method of assessing
#' change in a single subject: An alteration of the RC index. \emph{Behavior
#' Therapy, 17}, 305-308.
#' 
#' Hageman, W. J. J., & Arrindell, W. A. (1993). A further refinement of the
#' reliable change (RC) index by improving the pre-post difference score:
#' Introducing RCID. \emph{Behaviour Research and Therapy, 31}, 693-700.
#' 
#' Jacobson, N. S., & Truax, P. (1991). Clinical Significance: A statistical
#' approach to defining meaningful change in psychotherapy research.
#' \emph{Journal of Consulting and Clinical Psychology, 59}, 12-19.
#' 
#' Wise, E. A. (2004). Methods for analyzing psychotherapy outcomes: A review
#' of clinical significance, reliable change, and recommendations for future
#' directions. \emph{Journal of Personality Assessment, 82}, 50 - 59.
#' @examples
#' 
#' ## Report the RCIs of the first case from the byHeart data and include a graph
#' rciSC(byHeart2011[1], graph = TRUE)
#' 
#' @export
rciSC <- function(data, dvar, pvar, rel = 0.80, ci = 0.95, graph = FALSE, phases = c(1, 2)) {
  
  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- scdf_attr(data, .opt$dv) else scdf_attr(data, .opt$dv) <- dvar
  if (missing(pvar)) pvar <- scdf_attr(data, .opt$phase) else scdf_attr(data, .opt$phase) <- pvar

  data <- .SCprepareData(data, na.rm = TRUE)
  data <- .keepphasesSC(data, phases = phases,pvar = pvar)$data
  
  N <- length(data)
  
  if(N > 1) {
    stop("Multiple single-cases are given. Calculations can only be applied to one single-case data set.\n")
  }
  
  A <- lapply(data, function(x) x[, dvar][x[, pvar] == "A"])
  B <- lapply(data, function(x) x[, dvar][x[, pvar] == "B"])
  A <- unlist(A)
  B <- unlist(B)
  sA <- sd(A)
  sB <- sd(B)
  mA <- mean(A)
  mB <- mean(B)
  nA <- length(A)
  nB <- length(B)
  n <- nA + nB
  SE.A <- sA * sqrt(1 - rel)
  SE.B <- sB * sqrt(1 - rel)
  stand.dif <- (mB-mA)/sd(c(A,B))
  RCI.1 <- (mB - mA) / SE.A
  RCI.2 <- (mB - mA) / sqrt(2 * SE.A * SE.A)
  RCI.3 <- (mB - mA) * rel + (mB - mA) * (1 - rel) / (sqrt(rel) * sqrt(2 * SE.A * SE.A))
  descriptives.ma <- matrix(
    c(nA, nB, mA, mB, sA, sB, SE.A, SE.B), 2, 4, 
    dimnames = list(c("A-Phase", "B-Phase"), c("n", "mean", "SD", "SE"))
  ) 
  z <- qnorm(ci+0.5*(1-ci))
  ci.ma <- matrix(NA, 2, 2, byrow = TRUE, dimnames = list(c("A-Phase", "B-Phase"), c("Lower", "Upper")))
  ci.ma[1,1] <- mA - z * SE.A
  ci.ma[1,2] <- mA + z * SE.A
  ci.ma[2,1] <- mB - z * SE.B
  ci.ma[2,2] <- mB + z * SE.B
  
  if(graph) {
    dat <- cbind(ci.ma[1, ], ci.ma[2, ])
    colnames(dat) <- c("A-Phase", "B-Phase")
    main <- sprintf("%d%% confidence interval (rtt = %.2f)", ci * 100, rel)
    boxplot(dat, ylab = "Mean", main = main)
  }
  
  RCI.ma <- matrix(c(RCI.1, RCI.2, RCI.3), 3, 1, dimnames = list(c("Jacobson et al.", "Christensen and Mendoza", "Hageman and Arrindell"), "RCI"))
  out <- list(RCI = RCI.ma, stand.dif = stand.dif, conf = ci.ma, conf.percent = ci, reliability = rel, descriptives = descriptives.ma) 
  class(out) <- c("sc","rci")
  
  out
}


rCi <- function(...){rciSC(...)}
