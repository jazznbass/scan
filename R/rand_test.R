#' Randomization Tests for single-case data
#'
#' The `rand_test` function computes a randomization test for single or multiple
#' baseline single-case data.  The function is based on an algorithm from the
#' `SCRT` package (Bulte & Onghena, 2009, 2012), but rewritten and extended for
#' the use in AB designs.
#'
#' # Details
#'
#' ## Predefinded statisic
#'
#' Use the `statistic` argument to choose a predefnied statistic. The
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
#'
#' ## Create own statistic function
#'
#' Use the `statistic_function` argument to proved your own function in a list.
#' This list must have an element named `statistic` with a function that takes
#' two arguments `a` and `b` and returns a single numeric value.  E.g.
#' `list(statistic = function(a, b) mean(a) - mean(b)`. A second element of the
#' list is named `aggregate` which takes a function with one numeric argument
#' that returns a numeric argument. This function is used to aggregate the
#' values of a multiple case design. If you do not provide this element, it uses
#' the default `function(x) sum(x)/length(x)`. The third optional argument is
#' `name` which provides a name for your user function.
#'
#' @inheritParams .inheritParams
#' @param statistic Defines the statistic on which the comparison of phases A
#'   and B is based on. Default setting is `statistic = "Mean B-A"`. See
#'   details.
#' @param statistic_function A list with a user defined function to calculate
#'   the statistic. When set, overwrites the `statistic` argument. See details.
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
#'
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
                                     "NAP", "NAP decreasing",
                                     "Slope B-A","Slope A-B"), 
                       statistic_function = NULL,
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
    
    by_call(statistic)
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
  mts  <- lapply(data, nrow)
  n_cases   <- length(data)
  
  testdirection <- "greater"
  
  if (identical(exclude.equal, "auto")) exclude.equal <- n_cases == 1
  
# starting points ---------------------------------------------------------

  if (length(limit) == 1) limit[2] <- limit[1]
  #obs.B.start <- unlist(lapply(a, function(x) length(x) + 1))
  
  if (is.na(startpoints[1])) {
    pos_startpts <- lapply(mts, function(x) (limit[1] + 1):(x - limit[2] + 1))
  } else {
    pos_startpts <- lapply(mts, function(x) startpoints)
  }
  
  ### posible combinations
  
  possible_combinations <- lapply(pos_startpts, length)
  possible_combinations <- cumprod(unlist(possible_combinations))[n_cases]	
  
  auto_corrected_number <- FALSE
  if (!complete && possible_combinations <= number) {
    auto_corrected_number <- TRUE
    complete <- TRUE
  }
  
  if (!complete) {
    startpts <- lapply(pos_startpts, function(x) sample(x, number, replace = TRUE))
    startpts <- matrix(unlist(startpts), nrow = number, ncol = n_cases)
  }
  if (complete) {
    startpts <- expand.grid(pos_startpts)
    number   <- nrow(startpts)
  }
  
# Sample Random A and B phases ---------------------------------------------
  
  rnd_a <- vector("list", number)
  for (i in 1:number) {
    ascores <- vector("list", n_cases)
    for (case in 1:n_cases)
      ascores[[case]] <- data[[case]][1:(startpts[i, case] - 1), dvar]
    rnd_a[[i]] <- ascores
  }

  rnd_b <- vector("list", number)
  for (i in 1:number) {
    ascores <- vector("list", n_cases)
    for (case in 1:n_cases)
      ascores[[case]] <- data[[case]][startpts[i, case]:mts[[case]], dvar]
    rnd_b[[i]] <- ascores
  }
  
# Functions for phase differences -----------------------------------------

  if (!is.null(statistic_function)) {
    
    if (is.null(statistic_function$aggregate)) 
      statistic_function$aggregate <- function(x) sum(x) / length(x)
    
    res <- rand_test_statistic(rnd_a, rnd_b, a, b,
      statistic = statistic_function$statistic,
      args_statistic = NULL,
      aggregate = statistic_function$aggregate
    )
    
    if (is.null(statistic_function$name)) statistic_function$name <- ""
    statistic <- paste0("user defined function ", statistic_function$name)
  }
  
  if (statistic == "SMD hedges") {
    res <- rand_test_statistic(rnd_a, rnd_b, a, b,
      statistic = .opt$rand_test$smd,
      args_statistic = list(method = "hedges"),
      aggregate = function(x) sqrt(sum(x^2) / length(x))
    )
  }  
  
  if (statistic == "SMD glass") {
    res <- rand_test_statistic(rnd_a, rnd_b, a, b,
      statistic = .opt$rand_test$smd,
      args_statistic = list(method = "glass"),
      aggregate = function(x) sqrt(sum(x^2) / length(x))
    )
  }  
  
  if (statistic == "W-test") {
    res <- rand_test_statistic(rnd_a, rnd_b, a, b, 
      statistic = .opt$rand_test$u_test,
      args_statistic = NULL,
      aggregate = function(x) sum(x) / length(x)
    )
    testdirection <- "less"
  }   

  if (statistic == "T-test") {
    res <- rand_test_statistic(rnd_a, rnd_b, a, b, 
      statistic = .opt$rand_test$t_test,
      args_statistic = NULL,
      aggregate = function(x) sum(x) / length(x)
    )
  } 
  
  if (statistic == "NAP") {
    res <- rand_test_statistic(rnd_a, rnd_b, a, b, 
      statistic = .opt$rand_test$nap,
      args_statistic = list(decreasing = FALSE),
      aggregate = function(x) sum(x) / length(x)
    )
  } 
  
  if (statistic == "NAP decreasing") {
    res <- rand_test_statistic(rnd_a, rnd_b, a, b, 
      statistic = .opt$rand_test$nap,
      args_statistic = list(decreasing = TRUE),
      aggregate = function(x) sum(x) / length(x)
    )
  } 
  
  if (statistic == "Mean B-A") {
    res <- rand_test_statistic(rnd_a, rnd_b, a, b, 
      statistic = .opt$rand_test$average,
      args_statistic = list(
        fn = function(x) {sum(x) / length(x)}, method = "B-A"
      ),
      aggregate = function(x) sum(x) / length(x)
    )
  } 
  
  if (statistic == "Mean A-B") {
    res <- rand_test_statistic(rnd_a, rnd_b, a, b, 
      statistic = .opt$rand_test$average,
      args_statistic = list(
        fn = function(x) {sum(x) / length(x)}, method = "A-B"
      ),
      aggregate = function(x) sum(x) / length(x)
    )
  } 
  
  if (statistic == "Mean |A-B|") {
    res <- rand_test_statistic(rnd_a, rnd_b, a, b, 
      statistic = .opt$rand_test$average,
      args_statistic = list(
        fn = function(x) {sum(x) / length(x)}, method = "abs"
      ),
      aggregate = function(x) sum(x) / length(x)
    )
  } 

  if (statistic == "Median B-A") {
    res <- rand_test_statistic(rnd_a, rnd_b, a, b, 
      statistic = .opt$rand_test$average,
      args_statistic = list(
        fn = function(x) {median(x)}, method = "B-A"
      ),
      aggregate = function(x) median(x)
    )
  } 
  
  if (statistic == "Median A-B") {
    res <- rand_test_statistic(rnd_a, rnd_b, a, b, 
      statistic = .opt$rand_test$average,
      args_statistic = list(
        fn = function(x) {median(x)}, method = "A-B"
      ),
      aggregate = function(x) median(x)
    )
  } 
  
  if (statistic == "Median |A-B|") {
    res <- rand_test_statistic(rnd_a, rnd_b, a, b, 
      statistic = .opt$rand_test$average,
      args_statistic = list(
        fn = function(x) {median(x)}, method = "abs"
      ),
      aggregate = function(x) median(x)
    )
  }   

  if (statistic == "Slope B-A") {
    res <- rand_test_statistic(rnd_a, rnd_b, a, b,
      statistic = .opt$rand_test$slope,
      args_statistic = list(method = "B-A"),
      aggregate = function(x) sum(x) / length(x)
    )
  }   
  
  if (statistic == "Slope A-B") {
    res <- rand_test_statistic(rnd_a, rnd_b, a, b,
      statistic = .opt$rand_test$slope,
      args_statistic = list(method = "B-A"),
      aggregate = function(x) sum(x) / length(x)
    )
  }   
  
# p value -----------------------------------------------------------------
  
  if (testdirection == "greater") {
    test <- if (!exclude.equal) res$dist >= res$obs_stat else res$dist > res$obs_stat
  } else {
    test <- if (!exclude.equal) res$dist <= res$obs_stat else res$dist < res$obs_stat
  }
  
  p_value <- sum(test) / number
  
# return ------------------------------------------------------------------

  if (graph){
    h <- hist(res$dist, plot = FALSE)
    lab <- paste0(round(h$counts / length(res$dist) * 100, 0), "%")
    xlim <- c(min(h$breaks,na.rm = TRUE), max(h$breaks, na.rm = TRUE))
    ylim <- round(max(h$counts * 1.2))
    if (res$obs_stat < xlim[1]) xlim[1] <- res$obs_stat
    if (res$obs_stat > xlim[2]) xlim[2] <- res$obs_stat
    hist(
      res$dist, xlab = statistic, labels = lab, xlim = xlim, ylim = c(0, ylim), 
      ylab = "Frequency", main = "Random distribution", col = "lightgrey"
    )
    abline(v = res$obs_stat, lty = 2, lwd = 2, col = "grey") 
    if (p_value < 0.5) pos <- 2 else pos <- 4
    text(res$obs_stat, ylim, "observed", pos = pos)
  }
  
  z <- (res$obs_stat - mean(res$dist, na.rm = TRUE)) / sd(res$dist, na.rm = TRUE)
  p_z_single <- if (testdirection == "greater") 1 - pnorm(z) else pnorm(z)
    
  possible_combinations <- cumprod(unlist(lapply(pos_startpts, length)))[n_cases]

  out <- list(
    statistic = statistic, 
    phases.A = keep$phases_A, 
    phases.B = keep$phases_B, 
    N = n_cases, 
    n1 = length(unlist(a)), 
    n2 = length(unlist(b)), 
    limit = limit, 
    startpoints = startpoints, 
    p.value = p_value, 
    number = number, 
    complete = complete, 
    observed.statistic = res$obs_stat, 
    Z = z, 
    p.Z.single = p_z_single, 
    distribution = res$dist, 
    distribution_startpoints = startpts,
    possible.combinations = possible_combinations, 
    auto.corrected.number = auto_corrected_number,
    exclude.equal = exclude.equal,
    testdirection = testdirection
  )
  
  class(out) <- c("sc_rand")
  attr(out, opt("phase")) <- pvar
  attr(out, opt("dv")) <- dvar
  attr(out, "casenames") <- names(data)
  out

}

rand_test_statistic <- function(rnd_a, rnd_b, a, b, 
                                statistic,
                                args_statistic,
                                aggregate) {
  
  n_cases <- length(rnd_a[[1]])
  number <- length(rnd_a)
  rnd_a <- unlist(rnd_a,recursive = FALSE)
  rnd_b <- unlist(rnd_b,recursive = FALSE)

  dat <- mapply(statistic, a = rnd_a, b = rnd_b, MoreArgs = args_statistic)
  ma <- matrix(dat, ncol = n_cases, nrow = number, byrow = TRUE)   
  dist <- apply(ma, 1, aggregate)
  
  dat <- mapply(statistic, a = a, b = b, MoreArgs = args_statistic)
  list(
    obs_stat = aggregate(dat), 
    dist = dist
  )
}  


# function derived from: https://stackoverflow.com/questions/40141738/is-there-a-fast-estimation-of-simple-regression-a-regression-line-with-only-int
.estimate_slope <- function (x, y) {
  
  ## centring
  yc <- y - sum(y) / length(y)
  xc <- x - sum(x) / length(x)
  
  ## fitting an intercept-free model: yc ~ xc + 0
  xty <- c(crossprod(xc, yc))
  xtx <- c(crossprod(xc))
  
  # slope
  xty / xtx
  
}

.opt$rand_test$statistic_slope <- statistic <- function(a, b, method) {
  slope_b <- .estimate_slope(1:length(b), b)
  slope_a <- .estimate_slope(1:length(a), a)
  if (method == "B-A") slope_b - slope_a else slope_a - slope_b
}

.opt$rand_test$nap <- function(a, b, decreasing) {
  
  if (!decreasing) pos <- sum(unlist(lapply(a, function(x) b > x)))
  if (decreasing) pos <- sum(unlist(lapply(a, function(x) b < x)))
  
  ties <- sum(unlist(lapply(a, function(x) x == b)))
  non_overlaps <- pos + (0.5 * ties)
  pairs <- length(a) * length(b)
  nap <- non_overlaps / pairs * 100
}

.opt$rand_test$u_test <- function(a, b) {
  suppressWarnings(wilcox.test(a,b)$statistic)
}  

.opt$rand_test$t_test <- function(a, b) {
  suppressWarnings(t.test(b, a)$statistic)
}  

.opt$rand_test$smd <- function(a, b, method) {
  # Hedges'g + Durlak correction or Glass' Delta
  
  mean_a <- sum(a) / length(a)
  mean_b <- sum(b) / length(b)
  sd_a <- sd(a)
  sd_b <- sd(b)
  n_a <- length(a)
  n_b <- length(b)
  
  if (method == "hedges") {
    dat <- (mean_b - mean_a) / 
      sqrt(((n_a - 1) * sd_a^2 + (n_b - 1) * sd_b^2) / (n_a + n_b - 2)) *
      (((n_a+n_b) - 3) / ((n_a+n_b) - 2.25) * sqrt(((n_a+n_b) - 2) / (n_a+n_b)))
  } else if (method == "glass") {
    dat <- (mean_b - mean_a) / sd_a
  }
}

.opt$rand_test$average <- function(a, b, fn, method) {
  if (method == "B-A") {
    fn(b) - fn(a)
  } else if (method == "A-B") {
    fn(a) - fn(b)
  } else if (method == "abs") {
    abs(fn(b) - fn(a))
  }
}
