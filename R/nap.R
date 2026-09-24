#' Nonoverlap of all Pairs (NAP)
#'
#' Proportion of all pairs of a phase A and a phase B measurement in which the
#' phase B measurement is the higher one, with ties counted as half (Parker &
#' Vannest, 2009).
#'
#' @details Every measurement of phase A is compared with every measurement of
#'   phase B. NAP is the number of pairs in the expected direction plus half the
#'   number of tied pairs, divided by the number of pairs. With
#'   `decreasing = TRUE` the expected direction is reversed. Missing values are
#'   dropped before the pairs are formed; a case without measurements in one of
#'   the phases gives no pairs and `NA` for every value.
#'
#'   NAP runs from 0 to 100 percent with 50 percent as the point of no effect.
#'   The rescaled NAP spreads it to -100 to 100 percent, where negative values
#'   indicate an effect in the opposite direction: \eqn{nap_{rescaled} = 2 *
#'   nap - 100}.
#'
#'   `w` and `p` come from a Wilcoxon rank sum test in the expected direction,
#'   computed with a normal approximation rather than the exact algorithm. `d`
#'   and `R\eqn{^2}` are the effect sizes Parker and Vannest derive from NAP:
#'   \eqn{d = 3.464 * (1 - \sqrt{(1 - nap) / 0.5})} and \eqn{R^2 = r^2} with
#'   \eqn{r = d / \sqrt{d^2 + 4}}.
#' @inheritParams .inheritParams
#' @return An object of class `sc_nap` with the element `nap`, a data frame
#'   holding one row per case:
#'  |  |  |
#'  | --- | --- |
#'  | `Case` | Name of the case. |
#'  | `NAP` | Percentage of non-overlapping pairs. |
#'  | `NAP Rescaled` | NAP rescaled to a range of -100 to 100. |
#'  | `Pairs` | Number of pairs. |
#'  | `Non-overlaps` | Number of non-overlapping pairs, ties counted as half. |
#'  | `Positives` | Number of pairs in the expected direction. |
#'  | `Ties` | Number of tied pairs. |
#'  | `w` | Statistic of the Wilcoxon rank sum test. |
#'  | `p` | P value of that test. |
#'  | `d` | Cohen's d derived from NAP. |
#'  | `R2` | Squared correlation derived from d. |
#' @author Juergen Wilbert
#' @family overlap functions
#' @references Parker, R. I., & Vannest, K. (2009). An improved effect size for
#'   single-case research: Nonoverlap of all pairs. *Behavior Therapy, 40*,
#'   357-367.
#' @examples
#' nap(exampleAB)
#'
#' # data that are expected to decrease in phase B
#' nap(exampleAB_decreasing, decreasing = TRUE)
#'
#' # pooling the two A and the two B phases of an ABAB design
#' nap(exampleABAB, phases = list(A = c(1, 3), B = c(2, 4)))
#' @order 1
#' @export
nap <- function(data, dvar, pvar,
                decreasing = FALSE,
                phases = c(1, 2)) {
  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- dv(data) else dv(data) <- dvar
  if (missing(pvar)) pvar <- phase(data) else phase(data) <- pvar
  
  data <- .prepare_scdf(data)
  data <- recombine_phases(data, phases = phases)$data
  
  casenames <- revise_names(data)
  
  .nap <- function(data) {
    
    values <- split(data[[dvar]], data[[pvar]])
    values$A <- values$A[!is.na(values$A)]
    values$B <- values$B[!is.na(values$B)]

    pairs <- length(values$A) * length(values$B)
    
    if (pairs == 0) {
      return(data.frame(
        NAP = NA,
        "NAP Rescaled" = NA,
        Pairs = pairs,
        "Non-overlaps" = NA,
        Positives = NA,
        Ties = NA,
        w = NA,
        p = NA,
        d = NA,
        "R\u00B2" = NA,
        check.names = FALSE
      ))
    }
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
