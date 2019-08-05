#' Empirical power analysis for single-case data
#' 
#' !! This function is deprecated. Please use the power_testSC fucntion !!
#' The \code{power.testSC} command conducts a Monte-Carlo study on the
#' test-power and alpha-error of a randomization-test and a
#' piecewise-regression model.  The distribution values of the Monte-Carlo
#' sample are either specified by the user or estimated based on actual data.
#' 
#' @aliases power.testSC
#' @inheritParams .inheritParams
#' @param stat Defines the tests the power analysis is computed for. The
#' default \code{stat = c("rand.test","plm")} computes a power analysis for the
#' \code{\link{randSC}} and the \code{\link{plm}} analyses. Further
#' possibilities are \code{"hplm"} for a hierarchiacal linear regression model
#' and \code{"plm.poisson"} for a generalized piecewise-regression model under
#' the assumption of poisson distributed errors.
#' @param test.parameter Indicates whether the power and alpha error for a
#' level effect, a slope effect, or both effects should be estimated. The
#' default setting \code{test.parameter = c("level", "slope")} requests both.
#' @param rand.test.stat Defines the statistic the randomization test is based
#' on. The first values stipulates the statistic for the level-effect
#' computation and the second value for the slope-effect computation. Default
#' is \code{rand.test.stat = c("Mean B-A","B")}. Please see
#' \code{\link{randSC}} for more information on the test statistics.
#' @param cases Number of cases per study.
#' @param rtt Reliability of the underlying simulated measurements. Default is
#' \code{rtt = 0.8}.
#' @param level Defines the level increase (effect size \emph{d}) at the
#' beginning of phase B.
#' @param slope Defines the increase in scores - starting with phase B -
#' expressed as effect size \emph{d} per MT. \code{slope = .1} generates an
#' incremental increase of 0.1 standard deviations per MT for all phase B
#' measurements.
#' @param MT Number of measurements (in each study).
#' @param B.start Phase B starting point. A single value (e.g., \code{B.start =
#' 6}) defines \code{B.start} for all studies and cases. A vector of starting
#' values is given with the chain command (e.g., \code{B.start = c(6, 7, 8)}).
#' A value between 0 and 1 is interpreted as a proportion (e.g., \code{B.start
#' = c(0.3, 0.5, 0.8)} would start phase B at 30, 50, and 80\% of the MTs).
#' @param trend Defines the effect size \emph{d} of a trend per MT added
#' across the whole data-set.
#' @param n_sim Number of sample studies created for the the Monte-Carlo study.
#' Default is \code{n = 100}
#' @param limit Minimal number of data points per phase in the sample. Default
#' is \code{limit = 5}.
#' @param m Mean of the sample distribution the data are drawn from.
#' @param s Standard deviation of the sample distribution the data are drawn
#' from.
#' @param startpoints Alternative to the \code{limit} parameter start points
#' exactly defines the possible start points of phase B (e.g.,
#' \code{startpoints = 4:9} restricts the phase B start points to measurements
#' 4 to 9. \code{startpoints} overruns the \code{limit} parameter.
#' @param extreme.p Probability of extreme values. \code{extreme.p = .05} gives
#' a five percent probability of an extreme value. Default is \code{extreme.p =
#' 0}.
#' @param extreme.d Range for extreme values, expressed as effect size
#' \emph{d}. \code{extreme.d = c(-7,-6)} uses extreme values within a range of
#' -7 and -6 standard deviations. Caution: the first value must be smaller than
#' the second, otherwise the procedure will fail. Default is \code{extreme.d =
#' c(-4,-3)}.
#' @param exclude.equal If set to \code{exclude.equal = FALSE}, random
#' distribution values equal to the observed distribution are counted as
#' null-hypothesis conform. That is, they decrease the probability of rejecting
#' the null-hypothesis (increase the p-value). Default is \code{exclude.equal =
#' "auto"}, which means \code{FALSE} for multiple baseline designs and
#' \code{TRUE} for one single-case.
#' @param alpha Alpha level used to calculate the proportion of significant
#' tests. Default is \code{alpha = 0.05}.
#' @param distribution Indicates whether the random sample is based on a
#' \code{"normal"} or a \code{"poisson"} distribution. Default is
#' \code{distribution = "normal"}.
#' @param silent If set \code{TRUE}, the results are not printed after
#' computation. Default is \code{silent = FALSE}.
#' @param parameters -
#' @author Juergen Wilbert
#' @seealso \code{\link{plm}}, \code{\link{randSC}}
#' @examples
#' 
#' ## Assume you want to conduct a single-case study with 15 MTs, using a highly reliable test,
#' ## an expected level effect of \eqn{d = 1.4}, and randomized start points between MTs 5
#' ## and 12 can you expect to identify the effect using plm or randomization test?
#' mc_par <- list(
#'   n_cases = 1, mt = 15, B.start = round(runif (300,5,12)), 
#'   rtt = 0.8, level = 1.4
#' )
#' res <- power.testSC(
#'   parameters = mc_par, 
#'   stat = c("rand.test","hplm"), 
#'   test.parameter = "level",
#'   startpoints = 5:12,
#'   n_sim = 100
#' )
#' ## Would you achieve higher power by setting up a MBD with three cases?
#' mc_par <- list(
#'   n_cases = 3, mt = 15, B.start = round(runif (300,5,12)), 
#'   rtt = 0.8, level = 1.4
#' )
#' power.testSC(
#'   parameters = mc_par, 
#'   stat = c("rand.test","hplm"), 
#'   test.parameter = "level", 
#'   startpoints = 5:12,
#'   n_sim = 10
#' )
#' 
#' @export
power.testSC <- function(data = NULL, dvar, pvar, mvar, parameters = NULL, stat = c("rand.test","plm"), 
                         test.parameter = c("level", "slope"), rand.test.stat = c("Mean B-A","B"), 
                         cases = NULL, rtt = NULL, level = NULL, slope = NULL, MT = NULL, 
                         B.start = NULL, trend = NULL, n_sim = 100, limit = 5,  
                         m = NULL, s = NULL, startpoints = NA, extreme.p = 0, extreme.d = c(-4,-3), 
                         exclude.equal = "auto", alpha = 0.05, 
                         distribution = "normal", silent = TRUE) {
  
  warning(.opt$function_deprecated_warning, "Please use the power_testSC function.")
  n <- n_sim
  
  if (!is.null(data)) {
    
    # set attributes to arguments else set to defaults of scdf
    if (missing(dvar)) dvar <- scdf_attr(data, .opt$dv) else scdf_attr(data, .opt$dv) <- dvar
    if (missing(pvar)) pvar <- scdf_attr(data, .opt$phase) else scdf_attr(data, .opt$phase) <- pvar
    if (missing(mvar)) mvar <- scdf_attr(data, .opt$mt) else scdf_attr(data, .opt$mt) <- mvar
    
    data    <- .SCprepareData(data, change.var.phase = TRUE, change.var.values = TRUE, change.var.mt = TRUE)
    est     <- estimateSC(data, s = s, rtt = rtt)
    cases   <- length(data)
    B.start <- est$B.start
    MT      <- est$MT
    level   <- est$level
    slope   <- est$slope
    trend   <- est$trend
    m       <- est$m
    s       <- est$s
    rtt     <- est$rtt
  }
  if (!is.null(parameters)) {
    cases   <- parameters$n_cases
    B.start <- parameters$B.start
    MT      <- parameters$mt
    level   <- parameters$level
    slope   <- parameters$slope
    trend   <- parameters$trend
    m       <- parameters$m
    s       <- parameters$s
    rtt     <- parameters$rtt
  }
  
  if (is.null(rtt))   rtt   <- 0.8
  if (is.null(level)) level <- 0
  if (is.null(slope)) slope <- 0
  if (is.null(trend)) trend <- 0
  if (is.null(cases)) cases <- 1
  if (is.null(m))     m     <- 50
  if (is.null(s))     s     <- 10
  
  if (cases == 1 && is.null(s))
    stop("Standard deviation could not be estimated with less than two cases. Please provide a value.\n")

  if (any(stat %in% c("plm","plm.poissonm")) && cases > 1)
    stop("plm models can not be calculated with more than one case. Consider using hplm\n")
  
  if (exclude.equal == "auto") exclude.equal <- cases == 1
  
  if (!silent)	{
    cat("Compute Monte-Carlo power-analyses with the following parameters:\n\n")
    cat("Stats:\t\t", stat, "\n")
    cat("Sample studies\t", n, "\n")
    cat("Cases per sample", cases, "\n")
    cat("M\t\t", m, "\n")
    cat("SD\t\t", s, "\n")
    cat("MT\t\t", MT, "\n")
    cat("B.start\t\t", sort(unique(B.start)), "\n")
    cat("rtt\t\t", rtt, "\n")
    cat("d level\t\t", level, "\n")
    cat("d slope\t\t", slope, "\n")
    cat("d trend\t\t", trend, "\n")	
    cat("Extreme.p\t", extreme.p, "\n")	
    cat("Extreme.d\t", extreme.d, "\n")	
    cat("Alpha level\t", alpha, "\n")	
    cat("Exclude equal\t", exclude.equal, "\n")
    if (is.na(startpoints[1])) {
      cat("Limit\t\t", limit, "\n")
    } else {
      cat("Startpoints\t\t", startpoints, "\n")
    }
  }

  test_rand        <- any(stat == "rand.test")
  test_plm         <- any(stat == "plm")
  test_plm.poisson <- any(stat == "plm.poisson")
  test_hplm        <- any(stat == "hplm")
  test_tauU        <- any(stat == "tauU")  
  
  out <- list()
  out$power.tauU.level             <- NA
  out$power.ranlevel               <- NA
  out$power.plm.level              <- NA
  out$power.plm.slope              <- NA
  out$power.plm.poisson.level      <- NA
  out$power.plm.poisson.slope      <- NA
  out$power.hplm.level             <- NA
  out$power.hplm.slope             <- NA
  out$alphaerror.tauU.level        <- NA
  out$alphaerror.ranlevel          <- NA
  out$alphaerror.plm.level         <- NA
  out$alphaerror.plm.slope         <- NA
  out$alphaerror.plm.poisson.level <- NA
  out$alphaerror.plm.poisson.slope <- NA
  out$alphaerror.hplm.level        <- NA
  out$alphaerror.hplm.slope        <- NA
  
  out$rand.test.stat <- rand.test.stat
  out$rand.sample    <- NA
  
  mc_fun <- list(
    plm_level = function(x) .plm.mt(x, type = "level p"),
    plm_slope = function(x) .plm.mt(x, type = "slope p"),
    plm_poisson_level = function(x) .plm.mt(x, count.data = TRUE, type = "level p"),
    plm_poisson_slope = function(x) .plm.mt(x, count.data = TRUE, type = "slope p"),
    hplm_level = function(x) summary(hplm(x, random.slopes = FALSE, ICC = FALSE)$hplm)$tTable[3, 5],
    hplm_slope = function(x) summary(hplm(x, random.slopes = FALSE, ICC = FALSE)$hplm)$tTable[4, 5],
    tauU = function(x) tauUSC(x, method = "parker")$table[[1]][6, 12]
  )
  
  design <- design_rSC(
    n = cases, MT = MT, m = m, s = s,
    B.start = B.start, level = level, 
    slope = slope, trend = trend, extreme.p = extreme.p, 
    extreme.d = extreme.d, rtt = rtt, 
    distribution = distribution
  )
  design_no_level <- design_rSC(
    n = cases, MT = MT, m = m, s = s,
    B.start = B.start, level = 0, 
    slope = slope, trend = trend, extreme.p = extreme.p, 
    extreme.d = extreme.d, rtt = rtt, 
    distribution = distribution
  )
  design_no_slope <- design_rSC(
    n = cases, MT = MT, m = m, s = s,
    B.start = B.start, level = level, 
    slope = 0, trend = trend, extreme.p = extreme.p, 
    extreme.d = extreme.d, rtt = rtt, 
    distribution = distribution
  )
  
  res <- .power.testSC(
    design = design, n_sim = n_sim,
    rand.test.stat = rand.test.stat, 
    alpha = alpha, limit = limit,  
    startpoints = startpoints, exclude.equal = exclude.equal, 
    stat = stat, 
    test.parameter = test.parameter, 
    mc_fun = mc_fun
  )


  if (all(level == 0)) {
    if (test_rand) out$alphaerror.ranlevel <- res$ranlevel
    if (test_plm) out$alphaerror.plm.level <- res$plm.level
    if (test_plm.poisson) out$alphaerror.plm.poisson.level <- res$plm.poisson.level
    if (test_hplm) out$alphaerror.hplm.level <- res$hplm.level
    if (test_tauU) out$alphaerror.tauU.level <- res$tauU.level
  } else {
    if (test_rand) out$power.ranlevel <- res$ranlevel
    if (test_plm) out$power.plm.level <- res$plm.level
    if (test_plm.poisson) out$power.plm.poisson.level <- res$plm.poisson.level
    if (test_hplm) out$power.hplm.level <- res$hplm.level
    if (test_tauU) out$power.tauU.level <- res$tauU.level
  }

  if (all(slope == 0)) {
    if (test_plm) out$alphaerror.plm.slope <- res$plm.slope
    if (test_plm.poisson) out$alphaerror.plm.poisson.slope <- res$plm.poisson.slope
    if (test_hplm) out$alphaerror.hplm.slope <- res$hplm.slope
  } else {
    if (test_plm) out$power.plm.slope <- res$plm.slope
    if (test_plm.poisson) out$power.plm.poisson.slope <- res$plm.poisson.slope
    if (test_hplm) out$power.hplm.slope <- res$hplm.slope
  }
  
  if (any(level != 0)) {
    res <- .power.testSC(design = design_no_level, n_sim = n_sim,
      rand.test.stat = rand.test.stat, 
      alpha = alpha, limit = limit, startpoints = startpoints, 
      exclude.equal = exclude.equal, stat = stat, 
      test.parameter = test.parameter, 
      mc_fun = mc_fun
    )
    if (test_rand) out$alphaerror.ranlevel <- res$ranlevel
    if (test_plm) out$alphaerror.plm.level <- res$plm.level
    if (test_plm.poisson) out$alphaerror.plm.poisson.level <- res$plm.poisson.level
    if (test_hplm) out$alphaerror.hplm.level <- res$hplm.level
    if (test_tauU) out$alphaerror.tauU.level <- res$tauU.level
  }
  
  if (any(slope != 0)) {
    res <- .power.testSC(design = design_no_slope, n_sim = n_sim,
      rand.test.stat = rand.test.stat,
      alpha = alpha, limit = limit, startpoints = startpoints, 
      exclude.equal = exclude.equal, stat = stat, 
      test.parameter = test.parameter, 
      mc_fun = mc_fun
    )
    if (test_plm) out$alphaerror.plm.slope <- res$plm.slope
    if (test_plm.poisson) out$alphaerror.plm.poisson.slope <- res$plm.poisson.slope
    if (test_hplm) out$alphaerror.hplm.slope <- res$hplm.slope
  }
  
  class(out) <- c("sc","power")
  out
}

.power.testSC <- function(design, rand.test.stat = NA, alpha = NA, n_sim,
                          limit = NA, startpoints = NA,  exclude.equal = NA, 
                          stat, test.parameter, 
                          mc_fun) {
  
  out <- list()
  out$ranlevel          <- NA
  out$plm.slope         <- NA
  out$plm.level         <- NA
  out$plm.poisson.slope <- NA
  out$plm.poisson.level <- NA
  out$hplm.slope        <- NA
  out$hplm.level        <- NA
  out$tauU.level        <- NA
  out$rand.sample       <- NA
  
  test_rand        <- any(stat == "rand.test")
  test_plm         <- any(stat == "plm")
  test_plm.poisson <- any(stat == "plm.poisson")
  test_hplm        <- any(stat == "hplm")
  test_tauU        <- any(stat == "tauU")
  test_level       <- any(test.parameter == "level")
  test_slope       <- any(test.parameter == "slope")
  
  rand.sample <- list()
  for(i in 1:n_sim) rand.sample[[i]] <- rSC(design = design)
  
  # analyse random sample ---------------------------------------------------
  if (test_rand && test_level) {
    out$ranlevel <- .prop_sig(rand.sample, alpha, function(x)
      randSC(x, statistic = rand.test.stat[1], number = 100, 
             exclude.equal = exclude.equal, limit = limit, 
             startpoints = startpoints, output = "p")
    )
  }
  
  if (test_tauU && test_level) {
    out$tauU.level <- .prop_sig(rand.sample, alpha, mc_fun[["tauU"]])
  }
  
  if (test_plm) {
    if (test_level) {
      out$plm.level <- .prop_sig(rand.sample, alpha, mc_fun[["plm_level"]])
    }
    
    if (test_slope) {
      out$plm.slope <- .prop_sig(rand.sample, alpha, mc_fun[["plm_level"]])
    }
  }
  
  if (test_plm.poisson) {
    if (test_level) {
      out$plm.poisson.level <- .prop_sig(rand.sample, alpha, mc_fun[["plm_poisson_level"]])
    } 
    
    if (test_slope) {
      out$plm.poisson.slope <- .prop_sig(rand.sample, alpha, mc_fun[["plm_poisson_slope"]])
    }
  }
  
  if (test_hplm) {
    if (test_level) {
      out$hplm.level <- .prop_sig(rand.sample, alpha, mc_fun[["hplm_level"]])
    }
    if (test_slope) {
      out$hplm.slope <- .prop_sig(rand.sample, alpha, mc_fun[["plm_slope"]])
    }
  }
  
  out
}



