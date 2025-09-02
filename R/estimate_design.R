#' Estimate single-case design
#'
#' This functions takes an scdf and extracts design parameters. The resulting
#' object can be used to randomly create new scdf files with the same underlying
#' parameters. This is useful for Monte-Carlo studies and bootstrapping
#' procedures.
#'
#' @inheritParams .inheritParams
#' @param s The standard deviation depicting the between case variance of the
#'   overall performance. If more than two single-cases are included in the
#'   scdf, the variance is estimated if s is set to NULL.
#' @param rtt The reliability of the measurements. The reliability is estimated
#'   when rtt = NULL.
#' @param overall_rtt Ignored when `rtt` is set. If TRUE, rtt estimations will
#'   be based on all cases and identical for each case. If FALSE rtt is
#'   estimated for each case separately.
#' @param overall_effects If TRUE, trend, level, and slope effect estimations
#'   will be identical for each case. If FALSE, effects are estimated for each
#'   case separately.
#' @param ... Further arguments passed to the plm function used for parameter
#'   estimation.
#' @return A list of parameters for each single-case. Parameters include name,
#'   length, and starting measurement time of each phase, trend, level, and
#'   slope effects for each phase, start value, standard deviation, and
#'   reliability for each case.
#' @examples
#' # create a random scdf with predefined parameters
#' set.seed(1234)
#' design <- design(
#'   n = 10, trend = -0.02,
#'   level = list(0, 1), rtt = 0.8,
#'   s = 1
#' )
#' scdf<- random_scdf(design)
#'
#' # Estimate the parameters based on the scdf and create a new random scdf
#' # based on these estimations
#' design_est <- estimate_design(scdf, rtt = 0.8)
#' scdf_est <- random_scdf(design_est)
#'
#' # Analyze both datasets with an hplm model. See how similar the estimations
#' # are:
#' hplm(scdf, slope = FALSE)
#' hplm(scdf_est, slope = FALSE)
#'
#' # Also similar results for pand and randomization tests:
#' pand(scdf)
#' pand(scdf_est)
#' rand_test(scdf)
#' rand_test(scdf_est)
#' @export

estimate_design <- function(data, dvar, pvar, mvar, 
                            s = NULL, 
                            rtt = NULL, 
                            overall_effects = FALSE, 
                            overall_rtt = TRUE,
                            model = "JW", 
                            ...) {

  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- dv(data)
  if (missing(pvar)) pvar <- phase(data)
  if (missing(mvar)) mvar <- mt(data)
  dv(data) <- dvar
  phase(data) <- pvar
  mt(data) <- mvar
  
  data <- .prepare_scdf(data)
  N <- length(data)
  case_names <- names(data)

  if (is.null(case_names)) case_names <- revise_names(data)

  cases <- lapply(data, function(x) {
    df <- as.list(.phasestructure(x, pvar))
    names(df)[1:2] <- c("length", "phase")
    df
  })

  error <- c()
  fitted <- c()

  for (i in 1:N) {
    plm_model <- plm(data[i], model = model, ...)$full
    res <- coef(plm_model)
    var_fitted <- var(plm_model$fitted.values)
    var_residuals <- var(plm_model$residuals)
    n_phases <- length(cases[[i]]$phase)
    cases[[i]]$start_value <- res[1]
    cases[[i]]$trend <- res[2]
    cases[[i]]$level <- c(0, res[3:(1 + n_phases)])
    cases[[i]]$slope <- c(0, res[(2 + n_phases):(2 + 2 * (n_phases - 1))])
    
    if (is.null(rtt)) {
      cases[[i]]$rtt <- var_fitted / (var_fitted + var_residuals)
    } else {
      cases[[i]]$rtt <- rtt
    }
    
    cases[[i]]$missing_prop <- mean(is.na(data[[i]][[dvar]]))
    
    #missing_prop <- c()
    # for (y in 1:length(cases[[i]]$phase)) {
    #   missing_prop <- c(
    #     missing_prop, 
    #     sum(is.na(data[[i]][cases[[i]]$start[y]:cases[[i]]$stop[y], dvar])) / cases[[i]]$length[y]
    #   )
    # }
    #cases[[i]]$error <- var_residuals
    #cases[[i]]$fitted <- var_fitted
    
    error <- c(error, plm_model$residuals)
    fitted <- c(fitted, plm_model$fitted.values)
  }

  if (N > 2 && is.null(s)) 
    s <- sd(sapply(cases, function(x) x$start_value[1]), na.rm = TRUE)
  
  if (overall_effects) {
    level <- rowMeans(sapply(cases, function(x) x$level))
    trend <- mean(sapply(cases, function(x) x$trend))
    slope <- rowMeans(sapply(cases, function(x) x$slope))
    for (i in 1:N) {
      cases[[i]]$trend <- trend
      cases[[i]]$level <- level
      cases[[i]]$slope <- slope
    }
  }
  
  vars <- c(
    "phase", "length", "rtt", "missing_prop", "extreme_prop", 
    "extreme_low", "extreme_high", "trend", "level", "slope", "start_value", 
    "s", "start", "stop"
  )

  for (i in 1:N) {
    cases[[i]]$level <- cases[[i]]$level / s
    cases[[i]]$slope <- cases[[i]]$slope / s
    cases[[i]]$trend <- cases[[i]]$trend / s
    cases[[i]]$s <- s
    if (overall_rtt) cases[[i]]$rtt <- var(fitted) / (var(fitted) + var(error))
    cases[[i]]$extreme_prop <- 0
    cases[[i]]$extreme_low <- -4
    cases[[i]]$extreme_high <- -3
    cases[[i]] <- cases[[i]][vars]
  }

  out <- list(
    cases = cases,
    distribution = "normal"
  )

  class(out) <- c("sc_design")
  attributes(out)[opts("phase", "mt", "dv")] <- list(pvar, mvar, dvar)
  out
}
