#' Empirical power analysis for single-case data
#'
#' Conducts a Monte-Carlo study on the test-power and alpha-error probability of
#' a statistical function.
#'
#' Based on a \code{\link{design}} object, a large number of single-cases are
#' generated and re-analyzed with a provided statistical function. The
#' proportion of significant analyzes is the test power. In a second step, a
#' specified effect of the design object is set to 0 and again single-cases are
#' generated and reanalyzed. The proportion of significant analyzes is the alpha
#' error probability.
#'
#' @param design An object returned from the `design` function.
#' @param method A (named) list that defines the methods the power analysis is
#'   based on. Each element can contain a function (that takes an scdf file and
#'   returns a p value) or a character string (the name of predefined
#'   functions). default \code{method = list("plm_level", "rand", "tauU")}
#'   computes a power analysis based on \code{\link{tau_u}},
#'   \code{\link{rand_test}} and \code{\link{plm}} analyses. (Further predefined
#'   functions are: "plm_slope", "plm_poisson_level", "plm_poisson_slope",
#'   "hplm_level", "hplm_slope", "base_tau".
#' @param effect Either "level" or "slope". The respective effect of the
#'   provided design is set to 0 when computing the alpha-error proportion.
#' @param n_sim Number of sample studies created for the the Monte-Carlo study.
#'   Default is \code{n = 100}. Ignored if design_is_one_study = FALSE.
#' @param design_is_one_study If TRUE, the design is assumed to define all cases
#'   of one study that is repeatedly randomly created \code{n_sim} times. If
#'   false, the design is assumed to contain all cases from which a random
#'   sample is generated. This is useful for very specific complex simulation
#'   studies.
#' @param alpha_test Logical. If TRUE, alpha error is calculated.
#' @param power_test Logical. If TRUE, power is calculated.
#' @param binom_test Shortcut. When set TRUE, binom_test_power is set to 0.80,
#'   binom_test_alpha is set to 0.05, and binom_test_correct is set to 0.875.
#' @param binom_test_power Either FALSE or a value. If a value is provided, a
#'   binomial test is calculated testing if the power is greater than the
#'   provided value.
#' @param binom_test_alpha Either FALSE or a value. If a value is provided, a
#'   binomial test is calculated testing if the alpha error proportion is less
#'   than the provided value.
#' @param binom_test_correct Either FALSE or a value. If a value is provided, a
#'   binomial test is calculated testing if the correct proportion is greater
#'   than the provided value.
#' @param ci Either FALSE or a value. If a value is provided, confidence
#'   intervals at the provided level are calculated for power, alpha error, and
#'   correct proportions.
#' @param alpha_level Alpha level used to calculate the proportion of
#'   significant tests. Default is \code{alpha_level = 0.05}.
#' @author Juergen Wilbert
#' @seealso \code{\link{random_scdf}}, \code{\link{design}}
#' @examples
#'
#' ## Assume you want to conduct a single-case study with 15 measurements
#' ## (phases: A = 6 and B = 9) using a highly reliable test and
#' ## an expected level effect of d = 1.4.
#' ## A (strong) trend effect is trend = 0.05. What is the power?
#' ## (Note: n_sims is set to 10. Set n_sims to 1000 for a serious calculation.)
#' design <- design(
#'   n = 1, phase_design = list(A = 6, B = 9),
#'   rtt = 0.8, level = 1.4, trend = 0.05
#' )
#' power_test(design, n_sim = 10)
#'
#' ## Would you achieve higher power by setting up a MBD with three cases?
#' design <- design(
#'   n = 3, phase_design = list(A = 6, B = 9),
#'   rtt = 0.8, level = 1.4, trend = 0.05
#' )
#' power_test(design, n_sim=10, method=list("hplm_level", "rand", "tauU_meta"))
#' @export

power_test <- function(design,
                       method = c("plm_level", "rand", "tauU"), 
                       effect = "level",
                       n_sim = 100,
                       design_is_one_study = TRUE,
                       alpha_test = TRUE,
                       power_test = TRUE,
                       binom_test = FALSE,
                       binom_test_alpha = FALSE,
                       binom_test_power = FALSE,
                       binom_test_correct = FALSE,
                       ci = FALSE,
                       alpha_level = 0.05) {
  
  
  starttime <- proc.time()
  
  mc_fun <- unlist(
    lapply(
      method, 
      function(x) if (inherits(x, "character")) mc_function(x) else x),
      recursive = FALSE
  )
  
  # return object
  out <- data.frame(Method = names(mc_fun))
  
  # power calculation ----------
  
  if (power_test) {
    out$Power <- .mc_scdf(
      design = design,
      n_sim = n_sim,
      alpha_level = alpha_level,
      mc_fun = mc_fun,
      design_is_one_study = design_is_one_study
    )
  } else
    out$Power <- NA
  
  # alpha error calculation ----------
  
  if (alpha_test) {
    
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
    
    out$"Alpha Error" <- .mc_scdf(
      design = design_no_effect,
      n_sim = n_sim,
      alpha_level = alpha_level,
      mc_fun = mc_fun,
      design_is_one_study = design_is_one_study
    )
  } else out$"Alpha Error" <- NA
  
  out$"Alpha:Beta" <- sprintf("1:%.1f", (100 - out$Power) / out$"Alpha Error")
  out$Correct <- (out$Power + (100 - out$"Alpha Error")) / 2
  
  if (binom_test) {
    binom_test_alpha <- 0.05
    binom_test_power <- 0.80
    binom_test_correct <- 0.875
  }
  
  if (is.numeric(binom_test_power)) {
    b_test <- function(x) {
      x <- binom.test(round(x / 100 * n_sim), n_sim, p = binom_test_power, alternative = "greater")
      round(x$p.value)
    }
    out[["p_power"]] <- sapply(out$"Power", b_test)
  }
  
  if (is.numeric(binom_test_alpha)) {
    b_test <- function(x) {
      x <- binom.test(round(x / 100 * n_sim), n_sim, p = binom_test_alpha, alternative = "less")
      round(x$p.value)
    }
    out[["p_alpha"]] <- sapply(out$"Alpha Error", b_test)
  }
  
  if (is.numeric(binom_test_correct)) {
    b_test <- function(x) {
      x <- binom.test(round(x / 100 * n_sim * 2), n_sim * 2, p = binom_test_correct, alternative = "greater")
      round(x$p.value, 3)
    }
    out[["p_correct"]] <- sapply(out$Correct, b_test)
  }

  if (is.numeric(ci)) {
    b_test <- function(x) {
      x <- binom.test(round(x / 100 * n_sim), n_sim, p = binom_test, conf.level = ci, alternative = "two.sided")
      x$conf.int * 100
    }
    
    out[["Power lower"]] <- sapply(out$Power, function(x) b_test(x)[1])
    out[["Power upper"]] <- sapply(out$Power, function(x) b_test(x)[2])
    out[["Alpha Error lower"]] <- sapply(out$"Alpha Error", function(x) b_test(x)[1])
    out[["Alpha Error upper"]] <- sapply(out$"Alpha Error", function(x) b_test(x)[2])
      
    b_test <- function(x) {
      x <- binom.test(round(x / 100 * n_sim * 2), n_sim * 2, p = binom_test, alternative = "two.sided")
      x$conf.int * 100
    }
    out[["Correct lower"]] <- sapply(out$Correct, function(x) b_test(x)[1])
    out[["Correct upper"]] <- sapply(out$Correct, function(x) b_test(x)[2])
  }
  
  attr(out, "computation_duration") <- proc.time() - starttime
  attr(out, "binom_test_alpha") <- binom_test_alpha
  attr(out, "binom_test_power") <- binom_test_power
  attr(out, "binom_test_correct") <- binom_test_correct
  attr(out, "ci") <- ci
  class(out) <- c("sc_power")
  out
}

.mc_scdf <- function(design, 
                     alpha_level = NA, 
                     n_sim, 
                     mc_fun, 
                     design_is_one_study) {

  # Genrate random sample ----------------------------------------------------
  rand_sample <- list()
  
  if (design_is_one_study) {
    for(i in 1:n_sim) rand_sample[[i]] <- random_scdf(design = design)
  }
  
  if (!design_is_one_study) {
    tmp <- random_scdf(design = design)
    for (i in seq_along(tmp)) rand_sample[[i]] <- tmp[i]
  }
  
  # analyse random sample ---------------------------------------------------
  
  test_function <- function(func) {
    p <- sapply(rand_sample, func)
    mean(p <= alpha_level, na.rm = TRUE) * 100
  }
  out <-  sapply(mc_fun, test_function) 

  # return
  out
}

#' @rdname deprecated-functions
#' @export
power_testSC <- function(...) {
  .deprecated_warning("power_test", "power_testSC")
  power_test(...)
}
