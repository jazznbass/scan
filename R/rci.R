#' Reliable change index
#'
#' The `rci()` function computes indices of reliable change (Wise, 2004)
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
# #'   Hageman, W. J. J., & Arrindell, W. A. (1993). A further refinement of the
# #'   reliable change (RC) index by improving the pre-post difference score:
# #'   Introducing RCID. *Behaviour Research and Therapy, 31*, 693-700.
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
rci <- function(data, dvar, pvar, 
                rel, 
                ci = 0.95, 
                graph = FALSE, 
                phases = c(1, 2)) {
  
  check_args(
    at_most(length(data), 1, 
            "RCI can not be applied to more than one case."),
    within(rel, 0, 1),
    within(ci, 0, 1),
    is_logical(graph)
  )
  
  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- dv(data) else dv(data) <- dvar
  if (missing(pvar)) pvar <- phase(data) else phase(data) <- pvar
  
  data <- .prepare_scdf(data, na.rm = TRUE)
  data <- recombine_phases(data, phases = phases)$data
  
  N <- length(data)
  
  #if(N > 1) {
  #  stop("Multiple single-cases are given. Calculations can only be applied to one single-case data set.\n")
  #}
  
  A <- lapply(data, function(x) x[, dvar][x[, pvar] == "A"])
  B <- lapply(data, function(x) x[, dvar][x[, pvar] == "B"])
  A <- unlist(A)
  B <- unlist(B)
  sA <- sd(A, na.rm = TRUE)
  sB <- sd(B, na.rm = TRUE)
  mA <- mean(A, na.rm = TRUE)
  mB <- mean(B, na.rm = TRUE)
  nA <- sum(!is.na(A))
  nB <- sum(!is.na(A))
  n <- nA + nB
  seA <- sA * sqrt(1 - rel)
  seB <- sB * sqrt(1 - rel)
  stand_dif <- (mB-mA)/sd(c(A,B))
  se_dif <- sqrt(2*seA^2)
  
  cor_a_b <- rel
  xA <- mA
  xB <- mB
  rel_A <- rel
  rel_B <- rel
  
  rdd <- (sA^2*rel_A + sB^2*rel_B - 2*sA*sB*cor_a_b) / 
         (sA^2 + sB^2 - 2*sA*sB*cor_a_b)
  
  rci_jacobsen <- (mB - mA) / seA
  rci_christensen <- (mB - mA) / se_dif
  rci_hageman <- (xB - xA) * rdd + (mB - mA) * (1 - rdd) / 
    sqrt(seA^2 + seB^2)
  
  descriptives_ma <- matrix(
    c(nA, nB, mA, mB, sA, sB, seA, seB), 2, 4, 
    dimnames = list(c("A-Phase", "B-Phase"), c("n", "mean", "SD", "SE"))
  ) 
  z <- qnorm(ci + 0.5 * (1 - ci))
  ci_ma <- matrix(
    NA, 2, 2, byrow = TRUE, 
    dimnames = list(c("A-Phase", "B-Phase"), c("Lower", "Upper"))
  )
  ci_ma[1,1] <- mA - z * seA
  ci_ma[1,2] <- mA + z * seA
  ci_ma[2,1] <- mB - z * seB
  ci_ma[2,2] <- mB + z * seB
  
  if(graph) {
    dat <- cbind(ci_ma[1, ], ci_ma[2, ])
    colnames(dat) <- c("A-Phase", "B-Phase")
    main <- sprintf("%d%% confidence interval (rtt = %.2f)", ci * 100, rel)
    boxplot(dat, ylab = "Mean", main = main)
  }
  
  rci_ma <- matrix(
    c(rci_jacobsen, rci_christensen), 2, 1, 
    dimnames = list(
      c(
        "Jacobson et al.", 
        "Christensen and Mendoza"), 
        #"Hageman and Arrindell"), 
      "RCI"
    )
  )
  out <- list(
    rci = rci_ma, 
    stand_dif = stand_dif, 
    se_dif = se_dif,
    conf = ci_ma, 
    conf_percent = ci, 
    reliability = rel, 
    descriptives = descriptives_ma,
    rdd = rdd
  ) 
  class(out) <- c("sc_rci")
  attr(out, opt("phase")) <- pvar
  attr(out, opt("dv")) <- dvar
  out
}




