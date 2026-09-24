#' Baseline corrected tau
#'
#' Kendall's tau between the dependent variable and the phase variable, after
#' correcting for a trend in phase A. The correction is only applied when that
#' trend is significant, otherwise the uncorrected tau is reported.
#'
#' @details The procedure has been proposed by Tarlow (2016). Kendall's tau
#'   between the phase A values and the measurement times is taken as the
#'   baseline trend (the output labels this row `Baseline autocorrelation`).
#'   When its p value is at or below `alpha`, a non-parametric Theil-Sen
#'   regression of the values on the measurement times is fitted to phase A and
#'   extrapolated to all measurement times. The corrected tau is then Kendall's
#'   tau between the residuals of that extrapolation and the dichotomous phase
#'   variable. The Theil-Sen slope is the median of all pairwise slopes of
#'   phase A, the intercept the median of the residuals from that slope.
#'
#'   Corrected and uncorrected tau are always both computed and both reported
#'   per case; `alpha` only decides which of the two is taken as the result.
#'   With `continuity = TRUE` a continuity correction is applied to the z value
#'   and thus to the p value of all three taus.
#'
#'   Measurements with a missing value in the dependent variable or in the
#'   measurement-time variable are dropped beforehand. A case with fewer than
#'   two distinct measurement times in phase A, or with fewer than two
#'   observed measurements in phase A or fewer than one in phase B, gives `NA`
#'   throughout with a warning. If phase A has exactly two measurements or if
#'   all its values are identical, the baseline trend cannot be determined and
#'   is set to `NA` with a warning; no correction is applied in that case and
#'   the uncorrected tau becomes the result.
#' @inheritParams .inheritParams
#' @param alpha The p value of the baseline trend at and below which the
#'   baseline correction is applied.
#' @param continuity If TRUE, a continuity correction is applied when
#'   calculating z and p.
#' @param tau_method Character with values `"a"` or `"b"` indicating whether
#'   Kendall's Tau A or Tau B is applied.
#' @return An object of class `sc_bctau` with the elements:
#'  |  |  |
#'  | --- | --- |
#'  | `tau` | Resulting tau per case: the corrected one where the correction was applied, the uncorrected one otherwise. |
#'  | `p` | P value of `tau`. |
#'  | `correction` | Logical per case: was the baseline correction applied? |
#'  | `auto_tau` | Baseline trend per case. |
#'  | `tau_uncorrected` | Tau per case without baseline correction. |
#'  | `tau_corrected` | Tau per case with baseline correction. |
#'  | `corrected_tau` | One data frame per case with tau, z and p of all three models. |
#' @author Juergen Wilbert
#' @family regression functions
#' @references Tarlow, K. R. (2016). An Improved Rank Correlation Effect Size
#'   Statistic for Single-Case Designs: Baseline Corrected Tau. *Behavior
#'   Modification, 41(4)*, 427-467. https://doi.org/10.1177/0145445516676750
#' @examples
#' corrected_tau(exampleAB)
#'
#' # correct whenever the baseline trend reaches p <= .20
#' corrected_tau(exampleAB, alpha = 0.20)
#' @order 1
#' @export
corrected_tau <- function(data, dvar, pvar, mvar, 
                          phases = c(1, 2), 
                          alpha = 0.05, 
                          continuity = FALSE, 
                          tau_method = c("b", "a")) {
  
  # validity check ----
  check_args(
    by_call(tau_method),
    within(alpha, 0, 1),
    is_logical(continuity)
  )
  
  # prepare scdf ----
  if (missing(dvar)) dvar <- dv(data) else dv(data) <- dvar
  if (missing(pvar)) pvar <- phase(data) else phase(data) <- pvar
  if (missing(mvar)) mvar <- mt(data) else mt(data) <- mvar

  data <- .prepare_scdf(data)
  data <- recombine_phases(data, phases = phases)$data

  empty_return <- data.frame(
    Model = c(
      "Baseline autocorrelation",
      "Uncorrected tau",
      "Baseline corrected tau"
    ),
    tau = rep(NA_real_, 3),
    z = rep(NA_real_, 3),
    p = rep(NA_real_, 3),
    check.names = FALSE
  )

  corr_tau <- function(data) {
    
    # extract data for phase A and B ----
    data <- data[
      complete.cases(data[, c(dvar, mvar), drop = FALSE]), ,
      drop = FALSE
    ]

    rowsA <- which(data[[pvar]] == "A")
    rowsB <- which(data[[pvar]] == "B")

    A_data <- data[rowsA, ]
    B_data <- data[rowsB, ]
    
    # validity checks ----

    if (length(unique(A_data[[mvar]])) < 2L) {
      warn("Need at least two distinct measurement times in phase A.")
      return(empty_return)
    } else if (nrow(A_data) < 2L || nrow(B_data) < 1L) {
      warn(
        "Need at least two complete observations in phase A ",
        "and one in phase B."
      )
      return(empty_return)
    } else if (length(unique(A_data[[dvar]])) == 1) {
      warn(
        "All phase A values are identical. ",
        "Autocorrelation can not be calculated and is set to NA."
      )
      auto_tau <- list(tau = NA, z = NA, p = NA)
    } else if (length(A_data[[dvar]]) < 3 ) {
      warn(
        "Need at least three data points in phase A. ",
        "Autocorrelation can not be calculated and is set to NA."
      )
      auto_tau <- list(tau = NA, z = NA, p = NA)
    } else {
      auto_tau <- kendall_tau(
        A_data[[dvar]], 
        A_data[[mvar]], 
        continuity_correction = continuity,
        tau_method = tau_method
      )
    }
    
    # apply baseline correction if necessary ----

    formula  <- as.formula(paste0(dvar, "~", mvar))
    
    fit_ts <- theil_sen(formula, data = A_data)
    data$fit <- fit_ts$intercept + data[[mvar]] * fit_ts$slope
    
    y <- as.numeric(factor(data[[pvar]]))
    
    base_corr_tau <- kendall_tau(
      x = data[[dvar]] - data$fit, 
      y = y, 
      continuity_correction = continuity, 
      tau_method = tau_method
    )
    
    x <- data[[dvar]]
    
    uncorrected_tau <- kendall_tau(
      x = data[[dvar]], 
      y = y, 
      continuity_correction = continuity, 
      tau_method = tau_method
    )
    
    if (is.na(auto_tau$p)) {
      corr_applied <- FALSE
    } else {
      if (auto_tau$p <= alpha) {
        corr_applied <- TRUE
      } else {
        corr_applied <- FALSE
      }
    }
    
    if (corr_applied) tau <- base_corr_tau else tau <- uncorrected_tau
    
    # return results ----

    return(data.frame(
      Model = c("Baseline autocorrelation", 
                "Uncorrected tau", 
                "Baseline corrected tau"),
      tau = c(auto_tau$tau, uncorrected_tau$tau, base_corr_tau$tau),
      z = c(auto_tau$z, uncorrected_tau$z, base_corr_tau$z),
      p = c(auto_tau$p, uncorrected_tau$p, base_corr_tau$p),
      check.names = FALSE
    ))
  }
  
  x <- lapply(data, corr_tau)

  out <- list(
    tau = sapply(x, function(x) if(is.na(x$p[1]) || x$p[1] > alpha) x$tau[2] else x$tau[3]), 
    p = sapply(x, function(x) if(is.na(x$p[1]) || x$p[1] > alpha) x$p[2] else x$p[3]), 
    corrected_tau = x,
    auto_tau = sapply(x, function(x) x$tau[1]),
    tau_corrected = sapply(x, function(x) x$tau[3]),
    tau_uncorrected = sapply(x, function(x) x$tau[2]),    
    correction = sapply(x, function(x) if(is.na(x$p[1]) || x$p[1] > alpha) FALSE else TRUE),
    alpha = alpha,
    continuity = continuity,
    tau_method = tau_method,
    data = data
  )
  
  class(out) <- c("sc_bctau")
  attributes(out)[opts("phase", "mt", "dv")] <- list(pvar, mvar, dvar)
  out
}

theil_sen <- function(formula, data) {
  
  y <- model.frame(formula, data)[[1]]
  x <- model.frame(formula, data)[[2]]
  
  # Pairwise slopes (exclude ties in x)
  idx <- combn(length(y), 2)
  dx <- x[idx[2, ]] - x[idx[1, ]]
  dy <- y[idx[2, ]] - y[idx[1, ]]
  keep <- dx != 0
  
  #if (!any(keep)) abort("All x values are identical; slope is undefined.")

  #fitted <- intercept + slope * x
  #resid <- y - fitted
  
  slope <- median(dy[keep] / dx[keep])
  intercept <- median(y - slope * x)
  
  out <- list(
    intercept = intercept,
    slope = slope
  )
  
  out
}
