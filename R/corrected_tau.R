#' Baseline corrected tau
#'
#' Kendall's tau correlation for the dependent variable and the phase variable
#' is calculated after correcting for a baseline trend.
#'
#' @inheritParams .inheritParams
#' @param alpha Sets the p-value at and below which a baseline correction is
#'   applied.
#' @param continuity If TRUE applies a continuity correction for calculating p
#' @param repeated If TRUE applies the repeated median method for calculating
#'   slope and intercept ([mblm()])
#' @details This method has been proposed by Tarlow (2016). The baseline data
#'   are checked for a significant autocorrelation (based on Kendall's Tau). If
#'   so, a non-parametric Theil-Sen regression is applied for the baseline data
#'   where the dependent values are regressed on the measurement time. The
#'   resulting slope information is then used to predict data of the B-phase.
#'   The dependent variable is now corrected for this baseline trend and the
#'   residuals of the Theil-Sen regression are taken for further calculations.
#'   Finally, Kendall's tau is calculated for the dependent variable and the
#'   dichotomous phase variable. The function here provides two extensions to
#'   this procedure: The more accurate Siegel repeated median regression is
#'   applied when `repeated = TRUE` and a continuity correction is applied when
#'   `continuity = TRUE`.
#'
#' @family regression functions
#' @references Tarlow, K. R. (2016). An Improved Rank Correlation Effect Size
#'   Statistic for Single-Case Designs: Baseline Corrected Tau. *Behavior
#'   Modification, 41(4)*, 427–467. https://doi.org/10.1177/0145445516676750
#' @export
#'
#' @examples
#' dat <- scdf(c(A = 33,25,17,25,14,13,15, B = 15,16,16,5,7,9,6,5,3,3,8,11,7))
#' corrected_tau(dat)
corrected_tau <- function(data, dvar, pvar, mvar, 
                          phases = c(1, 2), 
                          alpha = 0.05, 
                          continuity = FALSE, 
                          repeated = FALSE) {
  
  if (missing(dvar)) dvar <- dv(data) else dv(data) <- dvar
  if (missing(pvar)) pvar <- phase(data) else phase(data) <- pvar
  if (missing(mvar)) mvar <- mt(data) else mt(data) <- mvar

  data <- .prepare_scdf(data, na.rm = TRUE)
  data <- recombine_phases(data, phases = phases)$data

  corr_tau <- function(data) {
    
    rowsA <- which(data[[pvar]] == "A")
    rowsB <- which(data[[pvar]] == "B")
    A_data <- data[rowsA, ]
    B_data <- data[rowsB, ]
    
    if (var(A_data[[dvar]]) == 0) {
      warning(
        "All phase A values are identical. ",
        "Autocorrelation can not be calculated and is set to NA.",
        call. = FALSE
      )
      auto_tau <- list(tau = NA, z = NA, p = NA)
    } else {
      auto_tau <- .kendall_full(
        A_data[[dvar]], 
        A_data[[mvar]], 
        continuity_correction = continuity
      )
    }
    
    formula  <- as.formula(paste0(dvar, "~", mvar))
    fit_mblm <- mblm(formula, dataframe = A_data, repeated = repeated)
    data$fit <- predict(fit_mblm, data, se.fit = FALSE)
    x <- data[[dvar]] - data$fit
    y <- as.numeric(factor(data[[pvar]]))
    base_corr_tau <- .kendall_full(x, y, continuity_correction = continuity)
    
    x <- data[[dvar]]
    uncorrected_tau <- .kendall_full(x, y, continuity_correction = continuity)
    
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
    
    df <- data.frame(
      Model = c("Baseline autocorrelation", 
                "Uncorrected tau", 
                "Baseline corrected tau"),
      tau = c(auto_tau$tau, uncorrected_tau$tau, base_corr_tau$tau),
      z = c(auto_tau$z, uncorrected_tau$z, base_corr_tau$z),
      p = c(auto_tau$p, uncorrected_tau$p, base_corr_tau$p),
      check.names = FALSE
    )
    
    df
    
  }
  
  x <- lapply(data, corr_tau)

  
  
  
  out <- list(
    tau = sapply(x, function(x) if(is.na(x$p[1]) || x$p[1] > 0.05) x$tau[2] else x$tau[3]), 
    p = sapply(x, function(x) if(is.na(x$p[1]) || x$p[1] > 0.05) x$p[2] else x$p[3]), 
    corrected_tau = x,
    auto_tau = sapply(x, function(x) x$tau[1]),
    tau_corrected = sapply(x, function(x) x$tau[3]),
    tau_uncorrected = sapply(x, function(x) x$tau[2]),    
    correction = sapply(x, function(x) if(is.na(x$p[1]) || x$p[1] > 0.05) FALSE else TRUE),
    alpha = alpha,
    continuity = continuity,
    repeated   = repeated,
    data = data
  )
  
  class(out) <- c("sc_bctau")
  attr(out, opt("phase")) <- pvar
  attr(out, opt("mt")) <- mvar
  attr(out, opt("dv")) <- dvar
  out
}
