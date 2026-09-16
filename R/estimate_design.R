#' Estimate single-case design
#'
#' This functions takes an scdf and extracts design parameters. The resulting
#' object can be used to randomly create new scdf files with the same underlying
#' parameters. This is useful for Monte-Carlo studies and bootstrapping
#' procedures.
#' 
#' The function uses the plm function to estimate parameters for each
#' single-case. If more than two single-cases are included in the scdf,
#' the between case variance depicting the overall performance (s) is
#' estimated unless s is provided. The reliability of the measurements (rtt)
#' is estimated for each case unless rtt is provided. If overall_rtt is set
#' to TRUE, rtt estimations will be based on all cases and identical for each
#' case. If overall_effects is set to TRUE, trend, level, and slope
#' effect estimations will be identical for each case.
#' 
#' `s` is estimated from the differences between the start values of the cases,
#' so it can only be estimated when the cases differ in their starting level by
#' more than the precision of these estimates — with one or two cases it can not
#' be estimated at all. When it can not, `s` is set equal to the standard
#' deviation of the error and a warning is issued: the effects are then
#' expressed in units of the variation within a case, and the reliability is
#' fixed at 0.5 by that choice rather than estimated. Providing `s`, `rtt` or
#' `error` is strongly recommended for such data. `s` and `rtt` are two sides of one
#' parameter: whichever `s` is used, the error distribution derived from the
#' estimated `rtt` has the residual variance of the piecewise regression, and
#' the effects are stored in units of `s` and multiplied by `s` again when data
#' are simulated. A different `s` therefore changes the reported parameters, not
#' the simulated data.
#' 
#' The resulting design object can be used as input for the random_scdf
#' function to create new random scdf files based on the estimated parameters.
#' This allows to create bootstrap samples or Monte-Carlo datasets based on
#' the characteristics of an existing dataset.
#'   
#' @inheritParams .inheritParams
#' @param s The standard deviation depicting the between case variance of the
#'   overall performance. If more than two single-cases are included in the
#'   scdf, the variance is estimated if s is set to NULL. The estimate is the
#'   variance of the estimated start values reduced by the mean squared standard
#'   error of these estimates, so that the uncertainty of the single estimates
#'   does not inflate `s`. When that difference is not positive, `s` is set equal
#'   to the standard deviation of the error and a warning is issued (see
#'   details). If s is provided, this value is used.
#' @param rtt The reliability of the measurements. The reliability is estimated
#'   when rtt = NULL, as `s^2 / (s^2 + var_error)`, the definition [design()]
#'   uses to draw the measurement error. `var_error` is the residual variance of
#'   the piecewise regression, pooled across cases when `overall_rtt = TRUE`. If
#'   rtt is provided, this value is used for all single-cases.
#' @param error Standard deviation of the measurement error. An alternative to
#'   `rtt`: the reliability of each case is then derived as
#'   `s^2 / (s^2 + error^2)`. `rtt` and `error` must not be given together. To
#'   assign different values to several single-cases, use a vector of values.
#' @param overall_rtt Ignored when `rtt` or `error` is set. If TRUE, rtt estimations will
#'   be based on all cases and identical for each case. If FALSE rtt is
#'   estimated for each case separately. Default is TRUE.
#' @param overall_effects If TRUE, trend, level, and slope effect estimations
#'   will be identical for each case. If FALSE, effects are estimated for each
#'   case separately. Default is FALSE.
#' @param ... Further arguments passed to the plm function used for parameter
#'   estimation. For example, the model argument can be used to select
#'   different piecewise regression models. See plm for details.
#' @return A list of parameters for each single-case. Parameters include name,
#'   length, and starting measurement time of each phase, trend, level, and
#'   slope effects for each phase, start value, standard deviation, and
#'   reliability for each case. This list can be used as input for the
#'   random_scdf function to create new random scdf files based on the estimated
#'   parameters.
#' @author Juergen Wilbert
#' @examples
#' # create a random scdf with predefined parameters
#' set.seed(1234)
#' design <- design(
#'   n = 10, trend = -0.02,
#'   level = list(0, 1), rtt = 0.8,
#'   s = 1, random_start_value = TRUE
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
                            error = NULL,
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
  
  if (!is.null(error)) {
    if (!is.null(rtt)) abort("Provide either 'rtt' or 'error', not both.")
    if (!is.numeric(error) || !all(is.finite(error)) || any(error <= 0)) {
      abort("Argument 'error' must hold positive numbers.")
    }
    error <- rep(error, length = N)
  }
  if (!is.null(rtt)) rtt <- rep(rtt, length = N)

  cases <- lapply(data, function(x) {
    df <- as.list(.phasestructure(x, pvar))
    names(df)[1:2] <- c("length", "phase")
    df
  })

  ss_residuals <- 0
  df_residuals <- 0

  for (i in 1:N) {
    plm_model <- plm(data[i], model = model, ...)$full
    res <- coef(plm_model)
    residual_values <- residuals(plm_model)
    df <- length(residual_values) - length(res)
 
    cases[[i]]$var_residuals <- sum(residual_values^2) / df
    cases[[i]]$var_start_value <- vcov(plm_model)[1, 1]
    n_phases <- length(cases[[i]]$phase)
    cases[[i]]$start_value <- res[1]
    cases[[i]]$trend <- res[2]
    cases[[i]]$level <- c(0, res[3:(1 + n_phases)])
    cases[[i]]$slope <- c(0, res[(2 + n_phases):(2 + 2 * (n_phases - 1))])
    cases[[i]]$missing_prop <- mean(is.na(data[[i]][[dvar]]))
    
    ss_residuals <- ss_residuals + sum(residual_values^2)
    df_residuals <- df_residuals + df
  }
  
  var_error <- ss_residuals / df_residuals

  if (is.null(s)) {
    var_s <- if (N > 2) {
      var(sapply(cases, function(x) x$start_value[1]), na.rm = TRUE) -
        mean(sapply(cases, function(x) x$var_start_value[1]), na.rm = TRUE)
    } else NA_real_
    
    if (isTRUE(var_s > 0)) {
      s <- sqrt(var_s)
    } else {
      s <- sqrt(var_error)
      warn("'s' can not be estimated from these data. It is set equal to the ",
           "standard deviation of the error (s = ", round(s, 3), ").")
      if (is.null(rtt) && is.null(error)) {
        warn("Equating the two fixes the reliability at rtt = 0.5 by ",
             "construction. It is not estimated from the data.")
      }
      warn("Level, slope and trend are in units of the within-case error. ",
           "Providing 's' or 'rtt' is strongly recommended.")
    }
  }
  
  if (!isTRUE(is.finite(s) && s > 0)) {
    abort("Argument 's' must be a positive number (is ", s, ").")
  }
  
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
    cases[[i]]$rtt <- if (!is.null(rtt)) {
      rtt[i]
    } else if (!is.null(error)) {
      s^2 / (s^2 + error[i]^2)
    } else if (overall_rtt) {
      s^2 / (s^2 + var_error)
    } else {
      s^2 / (s^2 + cases[[i]]$var_residuals)
    }
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
