#' Baseline corrected tau
#' 
#' Kendalls tau correlation for the dependent variable and the phase variable is calculated
#' after correcting for a baseline trend.
#'
#' @inheritParams .inheritParams
#' @param alpha Sets the p-value at and below which a baseline correction is applied.
#' @param continuity If TRUE applies a continuity correction for calculating p
#' @param repeated If TRUE applies the repeated median method for caluclating slope and intercept (\code{\link{mblm}})
#' @details This method has been proposed by Tarlow (2016). The baseline data are checked for a singificant 
#' autocorrelation (based on Kendalls Tau). If so, a non-parameteric Theil-Sen regression is applied
#' for the baseline data where the dependent values are regressed on the measurement time. The resulting slope
#' information is then used to predict data of the B-phase. The dependent variable is now corrected for this baseline trend 
#' and the resudials of the Theil-Sen regression are taken for further caluculations.
#' Finally, a tau is calculated for the dependent variable and the dichtomos phase variable.
#' The function here provides two extensions to this procedure: The more accurate Siegel repeated median regression
#' is applied when \code{repeated = TRUE} and a continuity correction is applied when \code{continuity = TRUE} (both are the default settings).
#' 
#' @family regression functions
#' @references Tarlow, K. R. (2016). An Improved Rank Correlation Effect 
#' Size Statistic for Single-Case Designs: Baseline Corrected Tau. Behavior Modification, 41(4), 427–467. https://doi.org/10.1177/0145445516676750
#' @family overlap functions
#' @export
#'
#' @examples
#' dat <- scdf(c(A = 33,25,17,25,14,13,15, B = 15,16,16,5,7,9,6,5,3,3,8,11,7))
#' corrected_tauSC(dat)

corrected_tauSC <- function(data, dvar, pvar, mvar, phases = c(1,2), alpha = 0.05, continuity = TRUE, repeated = TRUE) {
  
  if (missing(dvar)) dvar <- scdf_attr(data, .opt$dv)    else scdf_attr(data, .opt$dv)    <- dvar
  if (missing(pvar)) pvar <- scdf_attr(data, .opt$phase) else scdf_attr(data, .opt$phase) <- pvar
  if (missing(mvar)) mvar <- scdf_attr(data, .opt$mt)    else scdf_attr(data, .opt$mt)    <- mvar

  data <- .SCprepareData(data, na.rm = TRUE)
  data <- .keepphasesSC(data, phases = phases, pvar = pvar)$data
  
  if (length(data) > 1) {
    stop("Baseline corrected tau is not applicable for multiple cases.")
  }
  data <- data[[1]] 

  rowsA <- which(data[[pvar]] == "A")
  rowsB <- which(data[[pvar]] == "B")
  A_data <- data[rowsA, ]
  B_data <- data[rowsB, ]
  
  auto_tau <- .kendall(A_data[[dvar]], A_data[[mvar]], continuity_correction = continuity)
                       
  if (isTRUE(auto_tau$p <= alpha)) {
    formula  <- as.formula(paste0(dvar, "~", mvar))
    fit_mblm <- mblm(formula, dataframe = A_data, repeated = repeated)
    data$fit <- predict(fit_mblm, data, se.fit = FALSE)
    data$res <- data[[dvar]] - data$fit
    corr_applied <- TRUE
  } else {
    corr_applied <- FALSE
    data$res <- data[[dvar]]
  }
  x <- data$res
  y <- as.numeric(factor(data[[pvar]]))
  base_corr_tau <- .kendall(x, y, continuity_correction = continuity)
  
  out <- list(
    tau        = base_corr_tau$tau.b, 
    p          = base_corr_tau$p, 
    correction = corr_applied,
    continuity = continuity,
    repeated   = repeated,
    auto_tau   = auto_tau
  )
  class(out) <- c("sc", "base_corr_tau")
  out
}
