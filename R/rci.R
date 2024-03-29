#' Reliable change index
#'
#' The `rci()` function computes three indices of reliable change (Wise, 2004)
#' and corresponding descriptive statistics.
#'
#' @inheritParams .inheritParams
#' @param rel Reliability of the measure, used to compute the standard error.
#' @param ci Width of confidence interval as a decimal. Default is `ci = 0.95`
#'   applying a 95 percent confidence interval.
#' @param graph If set `TRUE`, a box plot of phase A and B scores is displayed.
#'   `graph = FALSE` by default.
#' @author Juergen Wilbert
#' @references Christensen, L., & Mendoza, J. L. (1986). A method of assessing
#'   change in a single subject: An alteration of the RC index. *Behavior
#'   Therapy, 17*, 305-308.
#'
#'   Hageman, W. J. J., & Arrindell, W. A. (1993). A further refinement of the
#'   reliable change (RC) index by improving the pre-post difference score:
#'   Introducing RCID. *Behaviour Research and Therapy, 31*, 693-700.
#'
#'   Jacobson, N. S., & Truax, P. (1991). Clinical Significance: A statistical
#'   approach to defining meaningful change in psychotherapy research.
#' *Journal of Consulting and Clinical Psychology, 59*, 12-19.
#'
#'   Wise, E. A. (2004). Methods for analyzing psychotherapy outcomes: A review
#'   of clinical significance, reliable change, and recommendations for future
#'   directions. *Journal of Personality Assessment, 82*, 50 - 59.
#'
#' @examples
#'
#' ## Report the RCIs of the first case from the byHeart data and include a graph
#' rci(byHeart2011[1], graph = TRUE, rel = 0.8)
#'
#' @export
rci <- function(data, dvar, pvar, rel, ci = 0.95, graph = FALSE, phases = c(1, 2)) {
  
  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- dv(data) else dv(data) <- dvar
  if (missing(pvar)) pvar <- phase(data) else phase(data) <- pvar

  data <- .prepare_scdf(data, na.rm = TRUE)
  data <- recombine_phases(data, phases = phases)$data
  
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
  RCI.3 <- (mB - mA) * rel + (mB - mA) * (1 - rel) / 
           (sqrt(rel) * sqrt(2 * SE.A * SE.A))
  descriptives.ma <- matrix(
    c(nA, nB, mA, mB, sA, sB, SE.A, SE.B), 2, 4, 
    dimnames = list(c("A-Phase", "B-Phase"), c("n", "mean", "SD", "SE"))
  ) 
  z <- qnorm(ci + 0.5 * (1 - ci))
  ci.ma <- matrix(
    NA, 2, 2, byrow = TRUE, 
    dimnames = list(c("A-Phase", "B-Phase"), c("Lower", "Upper"))
  )
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
  
  RCI.ma <- matrix(
    c(RCI.1, RCI.2, RCI.3), 3, 1, 
    dimnames = list(
      c("Jacobson et al.", "Christensen and Mendoza", "Hageman and Arrindell"), 
      "RCI"
    )
  )
  out <- list(
    RCI = RCI.ma, 
    stand.dif = stand.dif, 
    conf = ci.ma, 
    conf.percent = ci, 
    reliability = rel, 
    descriptives = descriptives.ma
  ) 
  class(out) <- c("sc_rci")
  attr(out, opt("phase")) <- pvar
  attr(out, opt("dv")) <- dvar
  out
}

#' @rdname deprecated-functions
#' @export
rciSC <- function(...) {
  rci(...)
}

#' @rdname deprecated-functions
#' @export
rCi <- function(...) {
  rci(...)
}
