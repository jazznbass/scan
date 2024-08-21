#' Between-Case Standardized Mean Difference
#' 
#' Calculates a standardized mean difference from a multilevel model as described in Pustejovsky et al. (2014)
#' 
#' @inheritParams .inheritParams
#' @param data Either an scdf or an object returned from the [hplm()] function.
#' @param ... When data is an scdf, further design parameters passed to the [hplm()] function.
#' @references 
#' Pustejovsky, J. E., Hedges, L. V., & Shadish, W. R. (2014). Design-Comparable Effect Sizes in Multiple Baseline Designs: A General Modeling Framework. Journal of Educational and Behavioral Statistics, 39(5), 368–393. https://doi.org/10.3102/1076998614547577
#' @examples
#' des <- design(
#'   n = 150,
#'   phase_design = list(A = 10, B = 10, A2 = 10, B2 = 10, c = 10),
#'   level = list(B = 1, A2 = 0, B2 = 1, c = 1)
#' )
#' study <- random_scdf(des)
#' between_smd(study)
#' 
#' model <- hplm(study, slope = FALSE, contrast_level = "preceding")
#' between_smd(model)
#' 
#' @order 1
#' @export
between_smd <- function(data, 
                        ...) {
  
  if (inherits(data, "sc_hplm")) {
    model <- data
  } else if (inherits(data, "scdf")) {
    model <- hplm(data, ...) 
  } 
  
  ids <- which(startsWith(names(model$hplm$coefficients$fixed), "phase"))
  a <- model$hplm$coefficients$fixed[ids]
  resid <- as.numeric(VarCorr(model$hplm)["Residual", "Variance"])
  inter <- as.numeric(VarCorr(model$hplm)["(Intercept)", "Variance"])
  bc_smd <- a / sqrt(sum(resid, inter))

  structure(
    list(
      bc_smd = data.frame(
        Effect = names(bc_smd), "BC-SMD" = bc_smd,# "Rand intercept" = a / sqrt(inter),
        check.names = FALSE
      ),
      model = model
    ),
    class = "sc_bcsmd"
  )
}