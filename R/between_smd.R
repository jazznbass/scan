#' Between-Case Standardized Mean Difference
#'
#' Calculates a standardized mean difference from a multilevel model as
#' described in Pustejovsky et al. (2014)
#'
#' The BC-SMD is calculate as `BC-SMD = Phase difference / sqrt(residual +
#' random_intercept)`. This is most closely related to Cohen's *d*. If you want
#' to have the most exact estimation based on the between case variance, you
#' have to exclude the residual variance by setting the argument
#' `include_residuals = FALSE` you get `BC-SMD = Phase difference /
#' sqrt(random_intercept)`. The 'base' model only includes the phase level as a
#' predictor like originally proposed by Hedges et al. Whereas the 'Full plm'
#' model includes the trend and the phase slope as additional predictors.

#' @param data Either an scdf or an object returned from the [hplm()] or
#'   [bplm()] function.
#' @param include_residuals Logical. See details.
#' @param model Either `"ml"` or `"bayesian"`.
#' @param ... Further arguments passed to the [hplm()] or [bplm()]function.
#' @return An object of class sc_bcsmd.
#' @references Pustejovsky, J. E., Hedges, L. V., & Shadish, W. R. (2014).
#'   Design-Comparable Effect Sizes in Multiple Baseline Designs: A General
#'   Modeling Framework. Journal of Educational and Behavioral Statistics,
#'   39(5), 368–393. https://doi.org/10.3102/1076998614547577
#' @examples
#' ## Create a example scdf:
#' des <- design(
#'   n = 150,
#'   phase_design = list(A1 = 10, B1 = 10, A2 = 10, B2 = 10, C = 10),
#'   level = list(B1 = 1, A2 = 0, B2 = 1, C = 1),
#'   rtt = 0.7,
#'   random_start_value = TRUE
#' )
#' study <- random_scdf(des)
#'
#' ## Standard BC-SMD return:
#' between_smd(study)
#'
#' ## Specify the model and provide an hplm object:
#' model <- hplm(study, contrast_level = "preceding", slope = FALSE,  trend = FALSE)
#' between_smd(model)
#'
#' ## excluding the residuals gives a more accruate estimation:
#' between_smd(model, include_residuals = FALSE)
#' 
#' @order 1
#' @export
between_smd <- function(data,
                        include_residuals = TRUE,
                        model = "frequentist",
                        ...) {
  
  models <- list()
  if (inherits(data, "sc_hplm")) {
    #message("BC-SMD calculation based on the provided hplm model.")
    model <- "frequentist"
    models[["Provided"]] <- data
  } else if (inherits(data, "sc_bplm")) {
    model <- "bayesian"
    models[["Provided"]] <- data
  } else if (inherits(data, "scdf")) {
    #message("BC-SMD calculated from an hplm model.")
    if (model == "frequentist") {
      models[["Base"]] <- hplm(
        data, 
        slope = FALSE, 
        trend = FALSE, 
        level = TRUE,
        ...
      ) 
      models[["Full plm"]] <- hplm(
        data, 
        slope = TRUE, 
        trend = TRUE, 
        level = TRUE,
        ...
      ) 
    }
    if (model == "bayesian") {
      models[["Base"]] <- bplm(
        data, 
        slope = FALSE, 
        trend = FALSE, 
        level = TRUE,
        ...
      ) 
      models[["Full plm"]] <- bplm(
        data, 
        slope = TRUE, 
        trend = TRUE, 
        level = TRUE,
        ...
      ) 
    }
  } 
  
  extract_bc_smd <- function(model) {
    ids <- which(startsWith(names(model$hplm$coefficients$fixed), "phase"))
    a <- model$hplm$coefficients$fixed[ids]
    resid <- as.numeric(nlme::VarCorr(model$hplm)["Residual", "Variance"])
    inter <- as.numeric(nlme::VarCorr(model$hplm)["(Intercept)", "Variance"])
    if (include_residuals) a / sqrt(sum(resid, inter)) else a / sqrt(inter)
  }
  
  extract_bc_smd_bayes <- function(model) {
    model <- summary(model$mcmcglmm)
    ids <- which(startsWith(row.names(model$solutions), "phase"))
    a <- model$solutions[ids, 1]
    resid <- model$Rcovariances["units", "post.mean"]
    inter <- model$Gcovariances["case", "post.mean"]
    if (include_residuals) a / sqrt(sum(resid, inter)) else a / sqrt(inter)
  }

  if (model == "frequentist") out <- lapply(models, extract_bc_smd)
  if (model == "bayesian") out <- lapply(models, extract_bc_smd_bayes)
  
  out <- as.data.frame(do.call(rbind, out), check.names = FALSE)
  
  out <- rownames_to_first_column(out, "Model")
  
  out <- list(
    bc_smd = out,
    include_residuals = include_residuals,
    model = model
  )
  class(out) <- "sc_bcsmd"
  attr(out, opt("phase")) <- attr(models[[1]], opt("phase"))
  attr(out, opt("mt"))    <- attr(models[[1]], opt("mt"))
  attr(out, opt("dv"))    <- attr(models[[1]], opt("dv"))
  out
}
