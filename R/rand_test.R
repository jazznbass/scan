#' Randomization Tests for single-case data
#'
#' The `rand_test` function computes a randomization test for single or multiple
#' baseline single-case data.  The function is based on an algorithm from the
#' `SCRT` package (Bulte & Onghena, 2009, 2012), but rewritten and extended for
#' the use in AB designs.
#'
#'
#' @inheritParams .inheritParams
#' @param statistic Defines the statistic on which the comparison of phases A
#'   and B is based on. Default setting is `statistic = "Mean B-A"`. The
#' following comparisons are possible: \itemize{ \item`Mean A-B`: Uses
#' the difference between the mean of phase A and the mean of phase B. This is
#' appropriate if a decrease of scores was expected for phase B.
#' \item`Mean B-A`: Uses the difference between the mean of phase B and
#' the mean of phase A. This is appropriate if an increase of scores was
#' expected for phase B.  \item`Mean |A-B|`: Uses the absolute value of
#' the difference between the means of phases A and B.  \item`Median
#' A-B`: The same as `Mean A-B`, but based on the median.
#' \item`Median B-A`: The same as `Mean B-A`, but based on the
#' median.  \item`SMD hedges / SMD glass`: Standardizes mean difference of B-A 
#' as Hedges's g or Glass' delta. \item`NAP`: Non-overlap of all pairs.
#' \item`W-test`: Wilcoxon-test statistic W.\item`T-test`: T-test statistic t.}
#' @param number Sample size of the randomization distribution. The exactness of
#'   the p-value can not exceed \eqn{1/number} (i.e., `number = 100` results in
#'   p-values with an exactness of one percent). Default is `number = 500`. For
#'   faster processing use `number = 100`. For more precise p-values set `number
#'   = 1000`).
#' @param complete If TRUE, the distribution is based on a complete permutation
#'   of all possible starting combinations. This setting overwrites the number
#'   Argument. The default setting is FALSE.
#' @param limit Minimal number of data points per phase in the sample. The first
#'   number refers to the A-phase and the second to the B-phase (e.g., `limit =
#'   c(5,3)`). If only one number is given, this number is applied to both
#'   phases. Default is `limit = 5`.
#' @param startpoints Alternative to the `limit`-parameter `startpoints` exactly
#'   defines the possible start points of phase B (e.g., `startpoints = 4:9`
#'   restricts the phase B start points to measurements 4 to 9. `startpoints`
#'   overruns the `limit`-parameter.
#' @param exclude.equal If set to \code{exclude.equal = FALSE}, which is the
#'   default, random distribution values equal to the observed distribution are
#'   counted as null-hypothesis conform. That is, they decrease the probability
#'   of rejecting the null-hypothesis (increase the p-value).
#'   \code{exclude.equal} should be set to `TRUE` if you analyse one single-case
#'   design (not a multiple baseline data set) to reach a sufficient power. But
#'   be aware, that it increases the chance of an alpha-error.
#' @param graph If `graph = TRUE`, a histogram of the resulting distribution is
#'   plotted. It is `FALSE` by default. *Note: use the more versatile
#'   [plot_rand()] function instead.*
#' @param output (deprecated and not implemented)
#' @param seed A seed number for the random generator.
#' @return \item{statistic}{Character string from function call (see
#'   \code{Arguments} above).} \item{N}{Number of single-cases.}
#'   \item{n1}{Number of data points in phase A.} \item{n2}{Number of data
#'   points in phase B.} \item{limit}{Numeric from function call (see
#'   \code{Arguments} above).}
#' \item{startpoints}{A vector defining the start points passed from the
#' function call (see `Arguments` above).} \item{p.value}{P-value of the
#' randomization test for the given data.} \item{number}{Sample size of
#' randomization distribution from function call (see \code{Arguments} above).}
#' \item{complete}{Logical argument from function call (see \code{Arguments}
#' above).} \item{observed.statistic}{Test statistic observed for the given
#' single-case data. (see \code{statistic} in the \code{Arguments} above.)}
#' \item{Z}{Z-value of observed test statistic.} \item{p.z.single}{Probability
#' of z-value.} \item{distribution}{Test statistic distribution from randomized
#' data sets.} \item{possible.combinations}{Number of possible combinations
#' under the given restrictions.} \item{auto.corrected.number}{`TRUE`
#' indicates that a corrected number of combinations was used. This happens, if
#' the number of possible combinations (under the given restrictions) undercuts
#' the requested `number` of combinations.} \item{ecxlude.equal}{see argument
#' above}
#' @author Juergen Wilbert
#' @references Bulte, I., & Onghena, P. (2009). Randomization tests for
#'   multiple-baseline designs: An extension of the SCRT-R package.
#' *Behavior Research Methods, 41*, 477-485.
#'
#'   Bulte, I., & Onghena, P. (2012). *SCRT: Single-Case Randomization Tests*.
#'   Available from: \url{https://CRAN.R-project.org/package=SCRT}
#' @examples
#'
#' ## Compute a randomization test on the first case of the byHeart2011 data and include a graph
#' rand_test(byHeart2011[1], statistic = "Median B-A", graph = TRUE, seed = 123)
#'
#' ## Compute a randomization test on the Grosche2011 data using complete permutation
#' rand_test(Grosche2011, statistic = "Median B-A", complete = TRUE, limit = 4, seed = 123)
#'
#' @export
rand_test <- function (data, dvar, pvar, 
                       statistic = c("Mean B-A", "Mean A-B", "Median B-A", 
                                     "Median A-B", "Mean |A-B|", "Median |A-B|",
                                     "SMD hedges", "SMD glass", 
                                     "W-test", "T-test", 
                                     "NAP", "NAP decreasing"), 
                       number = 500, 
                       complete = FALSE, 
                       limit = 5, 
                       startpoints = NA, 
                       exclude.equal = FALSE, 
                       phases = c(1, 2), 
                       graph = FALSE, 
                       output = NULL, 
                       seed = NULL) {

  
  check_args(
    by_call(statistic, "rand_test")
  )
  statistic <- statistic[1]
  
  if(!is.null(seed)) set.seed(seed)
  
  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- dv(data)
  if (missing(pvar)) pvar <- phase(data) 
  dv(data) <- dvar
  phase(data) <- pvar
  
  data <- .prepare_scdf(data)
  
  keep <- recombine_phases(data, phases = phases)
  data <- keep$data
  
  a   <- lapply(data, function(x) x[x[, pvar] == "A", dvar])
  b   <- lapply(data, function(x) x[x[, pvar] == "B", dvar])
  obs <- lapply(data, function(x) x[, dvar])
  MT  <- lapply(data, nrow)
  N   <- length(data)
  
  testdirection <- "greater"
  
  if (identical(exclude.equal, "auto")) exclude.equal <- N == 1
  
# starting points ---------------------------------------------------------

  if (length(limit) == 1) limit[2] <- limit[1]
  obs.B.start <- unlist(lapply(a, function(x) length(x) + 1))
  
  if (is.na(startpoints[1])) {
    pos.startpts <- lapply(MT, function(x) (limit[1] + 1):(x - limit[2] + 1))
  } else {
    pos.startpts <- lapply(MT, function(x) startpoints)
  }
  
  ### posible combinations
  
  possible.combinations <- lapply(pos.startpts, length)
  possible.combinations <- cumprod(unlist(possible.combinations))[N]	
  
  auto.corrected.number <- FALSE
  if (!complete && possible.combinations <= number) {
    auto.corrected.number <- TRUE
    complete <- TRUE
  }
  
  if (!complete) {
    startpts <- lapply(pos.startpts, function(x) sample(x, number, replace = TRUE))
    startpts <- matrix(unlist(startpts), nrow = number, ncol = N)
  }
  if (complete) {
    startpts <- expand.grid(pos.startpts)
    number   <- nrow(startpts)
  }
  
# Sample Random A and B phases ---------------------------------------------
  
  rnd_a <- list()
  for (i in 1:number) {
    ascores <- list()
    for (case in 1:N)
      ascores[[case]] <- data[[case]][1:(startpts[i, case] - 1), dvar]
    rnd_a[[i]] <- ascores
  }
  
  rnd_b <- list()
  for (i in 1:number) {
    ascores <- list()
    for (case in 1:N)
      ascores[[case]] <- data[[case]][startpts[i, case]:MT[[case]], dvar]
    rnd_b[[i]] <- ascores
  }
  
# Functions for phase differences -----------------------------------------

  if (statistic == "SMD hedges") {
    res <- rand_test_smd(rnd_a, rnd_b, a, b, method = "hedges")
  }  
  
  if (statistic == "SMD glass") {
    res <- rand_test_smd(rnd_a, rnd_b, a, b, method = "glass")
  }  
  
  if (statistic == "W-test") {
    res <- rand_test_u_test(rnd_a, rnd_b, a, b)
    testdirection <- "less"
  }   

  if (statistic == "T-test") {
    res <- rand_test_t_test(rnd_a, rnd_b, a, b)
  } 
  
  if (statistic == "NAP") {
    res <- rand_test_nap(rnd_a, rnd_b, a, b)
  } 
  
  if (statistic == "NAP decreasing") {
    res <- rand_test_nap(rnd_a, rnd_b, a, b, decreasing = TRUE)
  } 
  
  if (statistic == "Mean B-A") {
    res <- rand_test_avg(
      rnd_a, rnd_b, a, b, 
      fn = function(x) mean(x, na.rm = TRUE),
      method = "B-A"
    )
  } 
  
  if (statistic == "Mean A-B") {
    res <- rand_test_avg(
      rnd_a, rnd_b, a, b, 
      fn = function(x) mean(x, na.rm = TRUE),
      method = "A-B"
    )
  } 
  
  if (statistic == "Mean |A-B|") {
    res <- rand_test_avg(
      rnd_a, rnd_b, a, b, 
      fn = function(x) mean(x, na.rm = TRUE),
      method = "abs"
    )
  } 

  if (statistic == "Median B-A") {
    res <- rand_test_avg(
      rnd_a, rnd_b, a, b, 
      fn = function(x) median(x, na.rm = TRUE),
      method = "B-A"
    )
  } 
  
  if (statistic == "Median A-B") {
    res <- rand_test_avg(
      rnd_a, rnd_b, a, b, 
      fn = function(x) median(x, na.rm = TRUE),
      method = "A-B"
    )
  } 
  
  if (statistic == "Median |A-B|") {
    res <- rand_test_avg(
      rnd_a, rnd_b, a, b, 
      fn = function(x) median(x, na.rm = TRUE),
      method = "abs"
    )
  }   
  
# p value -----------------------------------------------------------------

  if (testdirection == "greater") {
    test <- if (!exclude.equal) res$dist >= res$obs.stat else res$dist > res$obs.stat
  } else {
    test <- if (!exclude.equal) res$dist <= res$obs.stat else res$dist < res$obs.stat
  }
  
  p.value <- sum(test) / number
  
# return ------------------------------------------------------------------

  if (graph){
    h <- hist(res$dist, plot = FALSE)
    lab <- paste0(round(h$counts / length(res$dist) * 100, 0), "%")
    xlim <- c(min(h$breaks,na.rm = TRUE), max(h$breaks, na.rm = TRUE))
    ylim <- round(max(h$counts * 1.2))
    if (res$obs.stat < xlim[1]) xlim[1] <- res$obs.stat
    if (res$obs.stat > xlim[2]) xlim[2] <- res$obs.stat
    hist(
      res$dist, xlab = statistic, labels = lab, xlim = xlim, ylim = c(0, ylim), 
      ylab = "Frequency", main = "Random distribution", col = "lightgrey"
    )
    abline(v = obs.stat, lty = 2, lwd = 2, col = "grey") 
    if (p.value < 0.5) pos <- 2 else pos <- 4
    text(obs.stat, ylim, "observed", pos = pos)
  }
  
  Z <- (res$obs.stat - mean(res$dist, na.rm = TRUE)) / sd(res$dist, na.rm = TRUE)
  p.Z.single <- if (testdirection == "greater") 1 - pnorm(Z) else pnorm(Z)
    
  possible.combinations <- cumprod(unlist(lapply(pos.startpts, length)))[N]

  out <- list(
    statistic = statistic, 
    phases.A = keep$phases_A, 
    phases.B = keep$phases_B, 
    N = N, 
    n1 = length(unlist(a)), 
    n2 = length(unlist(b)), 
    limit = limit, 
    startpoints = startpoints, 
    p.value = p.value, 
    number = number, 
    complete = complete, 
    observed.statistic = res$obs.stat, 
    Z = Z, 
    p.Z.single = p.Z.single, 
    distribution = res$dist, 
    possible.combinations = possible.combinations, 
    auto.corrected.number = auto.corrected.number,
    exclude.equal = exclude.equal,
    testdirection = testdirection
  )
  
  class(out) <- c("sc_rand")
  attr(out, opt("phase")) <- pvar
  attr(out, opt("dv")) <- dvar
  attr(out, "casenames") <- names(data)
  out

}

rand_test_avg <- function(rnd_a, rnd_b, a, b, fn, method) {
  
  N <- length(rnd_a[[1]])
  number <- length(rnd_a)
  avg_b <- unlist(lapply(rnd_b, function(x) lapply(x, fn)))
  avg_a <- unlist(lapply(rnd_a, function(x) lapply(x, fn)))
  
  ma <- if (method == "B-A") {
    matrix(avg_b - avg_a, ncol = N, nrow = number, byrow = TRUE)
  } else if (method == "A-B") {
    matrix(avg_a - avg_b, ncol = N, nrow = number, byrow = TRUE)
  } else if (method == "abs") {
    matrix(abs(avg_a - avg_b), ncol = N, nrow = number, byrow = TRUE)
  }
  
  dist <- apply(ma, 1, fn)
  
  avg_b <- unlist(lapply(b, fn))
  avg_a <- unlist(lapply(a, fn))
  
  obs_stat <- if (method == "B-A") {
    avg_b - avg_a
  } else if (method == "A-B") {
    avg_a - avg_b
  } else if (method == "abs") {
    abs(avg_a - avg_b)
  }
  
  list(
    obs_stat = fn(obs_stat),
    dist = dist
  )
  
}


rand_test_smd <- function(rnd_a, rnd_b, a, b, method) {
  
  
  N <- length(rnd_a[[1]])
  number <- length(rnd_a)
  
  stats_a <- unlist(lapply(rnd_a, function(x) lapply(x, mean, na.rm = TRUE)))
  stats_b <- unlist(lapply(rnd_b, function(x) lapply(x, mean, na.rm = TRUE)))
  sds_a <- unlist(lapply(rnd_a, function(x) lapply(x, sd, na.rm = TRUE)))
  sds_b <- unlist(lapply(rnd_b, function(x) lapply(x, sd, na.rm = TRUE)))
  ns_a <- unlist(lapply(rnd_a, function(x) lapply(x, length)))
  ns_b <- unlist(lapply(rnd_b, function(x) lapply(x, length)))
  
  # Hedges'g + Durlak correction or Glass' Delta
  
  if (method == "hedges") {
    dat <- (stats_b - stats_a) / 
           sqrt(((ns_a - 1) * sds_a^2 + (ns_b - 1) * sds_b^2) / (ns_a + ns_b - 2)) *
           (((ns_a+ns_b) - 3) / ((ns_a+ns_b) - 2.25) * sqrt(((ns_a+ns_b) - 2) / (ns_a+ns_b)))
  } else if (method == "glass") {
    dat <- (stats_b - stats_a) / sds_a
  }
  
  ma <- matrix(
    dat, 
    ncol = N, 
    nrow = number, 
    byrow = TRUE
  )   
 
  dist <- apply(ma, 1, function(x) sqrt(sum(x^2) / length(x)))
  
  stats_a <- unlist(lapply(a, mean, na.rm = TRUE))
  stats_b <- unlist(lapply(b, mean, na.rm = TRUE))
  sds_a <- unlist(lapply(a, sd, na.rm = TRUE))
  sds_b <- unlist(lapply(b, sd, na.rm = TRUE)) 
  ns_a <- unlist(lapply(a, length))
  ns_b <- unlist(lapply(b, length))
  
  if (method == "hedges") {
    dat <- (stats_b - stats_a) / 
      sqrt(((ns_a - 1) * sds_a^2 + (ns_b - 1) * sds_b^2) / (ns_a + ns_b - 2)) *
      (((ns_a+ns_b) - 3) / ((ns_a+ns_b) - 2.25) * sqrt(((ns_a+ns_b) - 2) / (ns_a+ns_b)))
  } else if (method == "glass") {
    dat <- (stats_b - stats_a) / sds_a
  }

  list(
    obs_stat = sqrt(sum(dat^2) / length(dat)),
    dist = dist
  )
}

rand_test_t_test <- function(rnd_a, rnd_b, a, b) {
  
  
  N <- length(rnd_a[[1]])
  number <- length(rnd_a)
 
  rnd_a <- unlist(rnd_a,recursive = FALSE)
  rnd_b <- unlist(rnd_b,recursive = FALSE)
  
  dat <- mapply(
    function(a, b) {
      suppressWarnings(t.test(b,a)$statistic)
    }, 
    a = rnd_a, b = rnd_b
  )
  
  ma <- matrix(
    dat, 
    ncol = N, 
    nrow = number, 
    byrow = TRUE
  )   
  
  dist <- apply(ma, 1, mean, na.rm = TRUE)
  
  dat <- mapply(
    function(a, b) {
      suppressWarnings(t.test(b,a)$statistic)
    }, 
    a = a, b = b
  )
  
  list(
    obs_stat = mean(dat, na.rm = TRUE),
    dist = dist
  )
}

rand_test_u_test <- function(rnd_a, rnd_b, a, b) {
  
  
  N <- length(rnd_a[[1]])
  number <- length(rnd_a)
  
  rnd_a <- unlist(rnd_a,recursive = FALSE)
  rnd_b <- unlist(rnd_b,recursive = FALSE)
  
  dat <- mapply(
    function(a, b) {
      suppressWarnings(wilcox.test(a,b)$statistic)
    }, 
    a = rnd_a, b = rnd_b
  )
  
  ma <- matrix(
    dat, 
    ncol = N, 
    nrow = number, 
    byrow = TRUE
  )   
  
  dist <- apply(ma, 1, mean, na.rm = TRUE)
  
  dat <- mapply(
    function(a, b) {
      suppressWarnings(wilcox.test(a,b)$statistic)
    }, 
    a = a, b = b
  )
  
  list(
    obs_stat = mean(dat, na.rm = TRUE),
    dist = dist
  )
}

rand_test_nap <- function(rnd_a, rnd_b, a, b, decreasing = FALSE) {
 
  N <- length(rnd_a[[1]])
  number <- length(rnd_a)
  rnd_a <- unlist(rnd_a,recursive = FALSE)
  rnd_b <- unlist(rnd_b,recursive = FALSE)

  dat <- mapply(
    function(a, b) {
     
      if (!decreasing) {
        pos <- sum(unlist(lapply(a, function(x) b > x)))
      }
      if (decreasing) {
        pos <- sum(unlist(lapply(a, function(x) b < x)))
      }
      
      ties <- sum(unlist(lapply(a, function(x) x == b)))
      non_overlaps <- pos + (0.5 * ties)
      pairs <- length(a) * length(b)
      nap <- non_overlaps / pairs * 100
    }, 
    a = rnd_a, b = rnd_b
  )
  
  ma <- matrix(
    dat, 
    ncol = N, 
    nrow = number, 
    byrow = TRUE
  )   
  
  dist <- apply(ma, 1, mean, na.rm = TRUE)
  
  dat <- mapply(
    function(a, b) {
      if (!decreasing) {
        pos <- sum(unlist(lapply(a, function(x) b > x)))
      }
      if (decreasing) {
        pos <- sum(unlist(lapply(a, function(x) b < x)))
      }
    
      ties <- sum(unlist(lapply(a, function(x) x == b)))
      non_overlaps <- pos + (0.5 * ties)
      pairs <- length(a) * length(b)
      nap <- non_overlaps / pairs * 100
    }, 
    a = a, b = b
  )
  
  list(
    obs_stat = mean(dat, na.rm = TRUE),
    dist = dist
  )

}  
