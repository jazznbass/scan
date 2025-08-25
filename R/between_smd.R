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
#' @param method Either `"REML"` or "`MCMglmm"`. This indicated which
#'   statistical method is applied to calculate the model.
#' @param ci A numeric between 0 and 1 setting the width of the confidence
#'   interval (when method is REML) or the credible interval (when method is
#'   MCMCglmm). The default is `0.95` for a 95-percent interval.
#' @param include_residuals Logical. See details.  
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
#' ## excluding the residuals gives a more accurate estimation:
#' between_smd(model, include_residuals = FALSE)
#'
#' @order 1
#' @export
between_smd <- function(data,
                        method = c("REML", "MCMCglmm"),
                        ci = 0.95,
                        include_residuals = TRUE,
                        ...) {
  
  
  
  check_args(
    by_call(method)
  )
  
  method <- method[1]
  
  models <- list()
  if (inherits(data, "sc_hplm")) {
    if (data$N == 1) {
      stop("Between-Case Standardized Mean Difference can not be computed ",
           "with one case.", call. = FALSE)
    }
    method <- "REML"
    models[["Provided"]] <- data
  } else if (inherits(data, "sc_bplm")) {
    if (data$N == 1) {
      stop("Between-Case Standardized Mean Difference can not be computed ",
           "with one case.", call. = FALSE)
    }
    method <- "MCMCglmm"
    models[["Provided"]] <- data
  } else if (inherits(data, "scdf")) {
    if (length(data) == 1) {
      stop("Between-Case Standardized Mean Difference can not be computed ",
           "with one case.", call. = FALSE)
    }
    
    if (method == "REML") {
      models[["Base model"]] <- hplm(
        data, 
        slope = FALSE, 
        trend = FALSE, 
        level = TRUE,
        method = "REML",
        ...
      ) 
      models[["Full plm model"]] <- hplm(
        data, 
        slope = TRUE, 
        trend = TRUE, 
        level = TRUE,
        method = "REML",
        ...
      ) 
    }
    if (method == "MCMCglmm") {
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
  
  extract_bc_smd <- function(model, ci) {
    
    model <- summary(model$hplm)
    ids <- which(startsWith(rownames(model$tTable), "phase"))
    beta <- model$tTable[ids, "Value"]
    se_beta <- model$tTable[ids, "Std.Error"]
    var_within <- model$sigma^2
    var_between <- as.numeric(VarCorr(model)["(Intercept)", "Variance"])
    
    if (include_residuals) variance <- var_within + var_between 
    if (!include_residuals) variance <- var_between 
    
    bc_smd <- beta / sqrt(variance)
    se_bc_smd <- se_beta / sqrt(variance)
    z <- qnorm((1 - (1 - ci)/2))
    ci_lower <- bc_smd - z * se_bc_smd
    ci_upper <- bc_smd + z * se_bc_smd

    out <- list(
      "Effect" = rownames(model$tTable)[ids],
      "BC-SMD" = bc_smd,
      se = se_bc_smd,
      ci_lower = ci_lower,
      ci_upper = ci_upper
    )
   
    out
    #if (!include_residuals) beta / sqrt(var_within + var_between)
    
    #beta <- model$hplm$coefficients$fixed[ids]
    #resid <- as.numeric(nlme::VarCorr(model$hplm)["Residual", "Variance"])
    #inter <- as.numeric(nlme::VarCorr(model$hplm)["(Intercept)", "Variance"])
    #if (include_residuals) beta / sqrt(sum(resid, inter)) else beta / sqrt(inter)
   
  }
  
  extract_bc_smd_bayes <- function(model, ci) {
    model <- model$mcmcglmm
    
    ids <- which(startsWith(colnames(model$Sol), "phase"))
    # Extract posterior samples
    betas <- model$Sol[, ids, drop = FALSE]
    var_resid_samples <- model$VCV[, "units"]
    var_random_intercept_samples <- model$VCV[, "case"]
    
    if (include_residuals) variance <- var_resid_samples + var_random_intercept_samples 
    if (!include_residuals) variance <- var_random_intercept_samples 
    
    # Compute BC-SMD per iteration
    bc_smds <- betas / sqrt(variance)
    
    # Summarize posterior
    posterior_mean <- mean(bc_smds)
    posterior_ci <- quantile(bc_smds, probs = c((1 - ci) / 2, 1 - ((1 - ci) / 2)))
    
    out <- list(
      "Effect" = colnames(model$Sol[, ids, drop = FALSE]),
      "BC-SMD" = posterior_mean,
      "Sample size" = nrow(model$Sol),
      ci_lower = posterior_ci[1],
      ci_upper = posterior_ci[2]
    )
    return(out)
    ########
    # model <- summary(model$mcmcglmm)
    # ids <- which(startsWith(row.names(model$solutions), "phase"))
    # a <- setNames(model$solutions[ids, 1], rownames(model$solutions)[ids])
    # resid <- model$Rcovariances["units", "post.mean"]
    # inter <- model$Gcovariances["case", "post.mean"]
    # #if (include_residuals) a / sqrt(sum(resid, inter)) else a / sqrt(inter)
    
  }

  if (method == "REML") {
    out <- lapply(models, function(x) { 
      extract_bc_smd(x, ci) |> 
        as.data.frame(check.names = FALSE)
    })
  }
  
  if (method == "MCMCglmm") {
    out <- lapply(models, function(x) { 
      extract_bc_smd_bayes(x, ci) |> 
        as.data.frame(check.names = FALSE)
    })
  }
  
  out <- list(
    models = out,
    ci = ci,
    method = method
  )
 
  class(out) <- "sc_bcsmd"
  attr(out, opt("phase")) <- attr(models[[1]], opt("phase"))
  attr(out, opt("mt"))    <- attr(models[[1]], opt("mt"))
  attr(out, opt("dv"))    <- attr(models[[1]], opt("dv"))
  out
}
