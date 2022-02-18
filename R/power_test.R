#' Empirical power analysis for single-case data
#' 
#' The \code{power_test} command conducts a Monte-Carlo study on the
#' test-power and alpha-error of a set of single-cases. The distribution 
#' values of the Monte-Carlo sample are either specified by the user or 
#' estimated based on actual data.
#' 
#' @inheritParams .inheritParams
#' @param design An object created by design_rSC
#' @param stat Defines the tests the power analysis is based on. The
#'   default \code{stat = c("plm_level", "rand", "tauU")} computes a power 
#'   analysis based on \code{\link{tau_u}}, \code{\link{randSC}} and 
#'   \code{\link{plm}} analyses. Further possibilities are: "plm_slope", 
#'   "plm_poisson_level", "plm_poisson_slope", "hplm_level", "hplm_slope", 
#'   "base_tau".
#' @param effect Either "level" or "slope".
#' @param n_sim Number of sample studies created for the the Monte-Carlo study.
#'   Default is \code{n = 100}. Ignored if design_is_one_study = FALSE.
#' @param design_is_one_study If TRUE, the design is assumed to define all 
#'   cases of one study that is repeatedly randomly created \code{n_sim} times. 
#'   If false, the design is assumed to contain all cases that from which a 
#'   random sample is generated. This is useful for very specific complex 
#'   simulation studies.
#' @param alpha_test Logical. If TRUE, alpha error is tested.
#' @param power_test Logical. If TRUE, power is tested.
#' @param binom_test Either FALSE or a value. If value is provided, a binomial test is calculated for the total correct identifications (alpha and power) against the provided value.
#' @param alpha_level Alpha level used to calculate the proportion of 
#'   significant tests. Default is \code{alpha_level = 0.05}.
#' @author Juergen Wilbert
#' @seealso \code{\link{plm}}, \code{\link{randSC}}
#' @examples
#' 
#' ## Assume you want to conduct a single-case study with 15 MTs, using a highly reliable test,
#' ## an expected level effect of \eqn{d = 1.4}, and randomized start points between MTs 5
#' ## and 12 can you expect to identify the effect using plm or randomization test?
#' design <- design_rSC(
#'   n = 1, phase.design = list(A = 6, B = 9), 
#'   rtt = 0.8, level = 1.4
#' )
#' res <- power_test(design, n_sim = 10)
#' 
#' ## Would you achieve higher power by setting up a MBD with three cases?
#' design <- design_rSC(
#'   n = 3, phase.design = list(A = 6, B = 9), 
#'   rtt = 0.8, level = 1.4
#' )
#' res <- power_test(design, n_sim = 10, stat = c("hplm_level", "rand"))
#' 
#' @export

power_test <- function(design,
                       stat = c("plm_level", "rand", "tauU"), 
                       effect = "level",
                       n_sim = 100,
                       design_is_one_study = TRUE,
                       alpha_stat = TRUE,
                       power_stat = TRUE,
                       binom_test = 0.5,
                       alpha_level = 0.05) {
  
  if (class(stat) == "list") {
    if (class(stat[[1]]) == "function")
    mc_fun <- stat
  } 
  
  if (class(stat) == "character") {
    mc_fun <- .opt$mc_fun[which(names(.opt$mc_fun) %in% stat)]
  } 
  
  # return object
  out <- data.frame(Method = names(mc_fun))
  
  # power calculation ----------
  
  if (power_stat) {
    out$Power <- .power_test(
      design = design, n_sim = n_sim, 
      alpha_level = alpha_level, mc_fun = mc_fun, 
      design_is_one_study = design_is_one_study
    ) 
  } else out$Power <- NA
  
  # alpha error calculation ----------

  if (alpha_stat) {
    
    #level <- any(sapply(design$cases, function(x) x$level) != 0)
    #slope <- any(sapply(design$cases, function(x) x$slope) != 0)
    
    design_no_effect <- design
    if (effect == "level") {
      design_no_effect$cases <- lapply(
        design_no_effect$cases, 
        function(x) {x$level <- rep(0, length = length(x$length)); x}
      )
    }
    
    if (effect == "slope") {
      design_no_effect$cases <- lapply(
        design_no_effect$cases, 
        function(x) {x$slope <- rep(0, length = length(x$length)); x}
      )
    }
    
    out$"Alpha Error" <- .power_test(
      design = design_no_effect, n_sim = n_sim, 
      alpha_level = alpha_level, mc_fun = mc_fun, 
      design_is_one_study = design_is_one_study
    )
  } else out$"Alpha Error" <- NA
  
  out$"Alpha:Beta" <- sprintf("1:%.1f", (100 - out$Power) / out$Alpha)
  out$Correct <- (out$Power + (100 - out$"Alpha Error")) / 2
  
  if (!isFALSE(binom_test)) {
    
    b_test <- function(x) {
      x <- binom.test(round(x / 100 * n_sim * 2), n_sim * 2, p = binom_test)
      round(x$p.value, 3)
    }
    
    out$p <- sapply(out$Correct, b_test)

  }

  class(out) <- c("sc_power")
  out
}

.power_test <- function(design, 
                        alpha_level = NA, 
                        n_sim, 
                        mc_fun, 
                        design_is_one_study) {

  # Genrate random sample ----------------------------------------------------
  rand.sample <- list()
  
  if (design_is_one_study) {
    for(i in 1:n_sim) rand.sample[[i]] <- rSC(design = design)
  }
  
  if (!design_is_one_study) {
    tmp <- rSC(design = design)
    for (i in seq_along(tmp)) rand.sample[[i]] <- tmp[i]
  }
  
  # analyse random sample ---------------------------------------------------
  
  test_function <- function(func) {
    p <- sapply(rand.sample, func)
    mean(p <= alpha_level, na.rm = TRUE) * 100
  }
  out <-  sapply(mc_fun, test_function) 

  # return
  out
}

#' @rdname power_test
#' @export
power_testSC <- function(...) {
  .deprecated_warning("power_test", "power_testSC")
  power_test(...)
}

