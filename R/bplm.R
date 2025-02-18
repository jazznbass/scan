#' Bayesian Piecewise Linear Model
#'
#' Computes a bayesian (hierarchical) piecewise linear model based on a Markov 
#' chain Monte Carlo sampler.
#'
#' @inheritParams .inheritParams
#' @order 1
#' @param random_trend If TRUE, includes a random trend trend effect.
#' @param random_level If TRUE, includes a random level trend effect.
#' @param random_slope If TRUE, includes a random slope trend effect.
#' @param fixed Defaults to the fixed part of the standard piecewise regression
#'   model. The parameter phase followed by the phase name (e.g., phaseB)
#'   indicates the level effect of the corresponding phase. The parameter
#'   'inter' followed by the phase name (e.g., interB) adresses the slope effect
#'   based on the method provide in the model argument (e.g., "B&L-B"). The
#'   formula can be changed for example to include further L1 or L2 variables
#'   into the regression model.
#' @param random The random part of the model.
#' @param update_fixed An easier way to change the fixed model part
#'   (e.g., `. ~ . + newvariable`).
#' @param ... Further arguments passed to the mcmcglmm function.
#' @return An object of class `sc_bplm`.
#'  |  |  |
#'  | --- | --- |
#'  | `model` | List containing infromation about the applied model. |
#'  | `N` | Number of single-cases. |
#'  | `formula` |A list containing the fixed and the random formulas of the hplm model. |
#'  | `mcmglmm` | Object of class MCMglmm. |
#'  | `contrast` | List with contrast definitions. |
#' @author Juergen Wilbert
#' @family regression functions
#' @examples
#' # bplm(Leidig2018)
#' @export

bplm <- function(data, dvar, pvar, mvar, 
                 model = c("W", "H-M", "B&L-B"),
                 contrast_level = c("first", "preceding"),
                 contrast_slope = c("first", "preceding"),
                 trend = TRUE, 
                 level = TRUE, 
                 slope = TRUE, 
                 random_trend = FALSE, 
                 random_level = FALSE, 
                 random_slope = FALSE, 
                 fixed = NULL, 
                 random = NULL, 
                 update_fixed = NULL, 
                 ...) {

  check_args(
    by_call(model),
    by_call(contrast_level),
    by_call(contrast_slope)
  )
  model <- model[1]
  contrast_level <- contrast_level[1]
  contrast_slope <- contrast_slope[1]
  
  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- dv(data) else dv(data) <- dvar
  if (missing(pvar)) pvar <- phase(data) else phase(data) <- pvar
  if (missing(mvar)) mvar <- mt(data) else mt(data) <- mvar
  
  dat <- .prepare_scdf(data)
  
  N <- length(dat)
  out <- list()
  out$model$interaction.method  <- model
  out$N                         <- N

# interaction and dummy coding and L2 --------------------------------------

  dat_dummies <- .add_dummy_variables(
    data = dat, 
    model = model, 
    contrast_level = contrast_level, 
    contrast_slope = contrast_slope
  )
  dat <- as.data.frame(dat_dummies$data)

# create formulas ---------------------------------------------------------

  if (is.null(fixed)) {
    fixed <- .create_fixed_formula(
      dvar, 
      mvar, 
      slope, 
      level, 
      trend, 
      dat_dummies$var_phase, 
      dat_dummies$var_inter
    )
  }
  if (!is.null(update_fixed)) fixed <- update(fixed, update_fixed)
  
  if (is.null(random) && N > 1) random <- ~case
  
  if (any(random_trend, random_level, random_slope)) {
    random <- .create_random_formula(
      mvar, 
      random_slope, 
      random_level, 
      random_trend, 
      dat_dummies$var_phase, 
      dat_dummies$var_inter,
      syntax = "mcmc"
    )
  }
  
  out$formula <- list(fixed = fixed, random = random)
  
# mcmcglmm model ----------------------------------------------------------
 
  out$mcmcglmm <- MCMCglmm(
    fixed = fixed,
    random = random,
    data = dat,
    verbose = FALSE,
    ...
  )
  
# out ----
    
  out$model$fixed  <- fixed
  out$model$random <- random
  out$contrast <- list(level = contrast_level, slope = contrast_slope)
  
  class(out) <- c("sc_bplm")
  attr(out, opt("phase")) <- pvar
  attr(out, opt("mt"))    <- mvar
  attr(out, opt("dv"))    <- dvar
  
  out
}

#' @describeIn bplm Print results
#' @inheritParams print.sc
#' @order 2
#' @param x An object returned by [bplm()]
#' @export
print.sc_bplm <- function(x, digits = 3, ...) {
  cat("Bayesian Piecewise Linear Regression\n\n")
  cat("Contrast model: ", 
      x$model$interaction.method, " (", 
      paste0(names(x$contrast), ": ",x$contrast, collapse = ", "), 
      ")\n", sep = "")
  
  if (x$N > 1) cat(x$N, "Cases\n\n")
  
  out <- list()

  model_summary <- summary(x$mcmc)
  
  cat("Deviance Information Criteron:", model_summary$DIC, "\n\n")
  
  # B-structure -----
  
  cat("B-structure - Fixed effects (",deparse(model_summary$fixed.formula),")\n\n", sep = "")
  
  md <- as.data.frame(model_summary$solutions)
  colnames(md) <- c("B", "l-95% CI", "u-95% CI", "eff.samp", "p")
  row.names(md) <- rename_predictors(row.names(md), x)
  print(round_numeric(md, digits))
  
  # G-structure -----
  
  if (!is.null(x$model$random)) {
    cat(
      "\nG-Structure - Random effects (",
      deparse(model_summary$random.formula),
      ")\n\n", 
      sep = ""
    )
    
    #vcv <- x$mcmc$VCV[, grep("case", colnames(x$mcmc$VCV))]
    #G_structure <- apply(vcv, 2, function(x) {
    #  c(mean = mean(x), lower = quantile(x, 0.025), upper = quantile(x, 0.975))
    #})
    #G_structure <- as.data.frame(t(G_structure))
    #G_matrix <- posterior.mode(vcv) |> as.data.frame()
    G_structure <- model_summary$Gcovariances |> as.data.frame()
    
    row.names(G_structure) <- rename_predictors(row.names(G_structure), x)
    row.names(G_structure) <- gsub(".case", "", row.names(G_structure))
    row.names(G_structure) <- gsub("case", "Intercept", row.names(G_structure))
    G_structure <- cbind(Parameter = row.names(G_structure), G_structure)
    row.names(G_structure) <- NULL
    
    G_structure <- G_structure[, -5]
    
    if (nrow(G_structure) > 1) {
      matching_elements <- grepl("^(.*):\\1$", G_structure$Parameter)
      G_structure_variance <- G_structure[matching_elements, ]
      G_structure_covariance <- G_structure[!matching_elements, ]
      
      G_structure_variance$Parameter <- sub(":.*", "", G_structure_variance$Parameter)
      
      G_structure_covariance$Parameter <- sapply(
        strsplit(G_structure_covariance$Parameter, ":", fixed = TRUE), 
        function(y) paste(sort(y), collapse = ":")
      )
      
      G_structure_covariance <- 
        G_structure_covariance[duplicated(G_structure_covariance$Parameter), ]
      
      names(G_structure_variance) <- c("Parameter", "SD", "lower 95% CI", "upper 95% CI") 
      names(G_structure_covariance) <- c("Parameter", "Correlation", "lower 95% CI", "upper 95% CI") 
      
      #cat("Standard deviation\n")
      G_structure_variance[, 2:4] <- sqrt(G_structure_variance[, 2:4])
      
      print(
        round_numeric(G_structure_variance, 3), 
        row.names = FALSE, 
        ...
      )
      
      cat("\nCorrelation\n")
      
      #print(G_structure_covariance, row.names = FALSE, ...)
      cor_vars <- strsplit(G_structure_covariance$Parameter, ":", fixed = TRUE)
      
      std_covar <- sapply(cor_vars, function(x) {
        var1 <- which(G_structure_variance$Parameter == x[1])
        var2 <- which(G_structure_variance$Parameter == x[2])
        G_structure_variance$SD[var1] * G_structure_variance$SD[var2]
      })
      
      
      for (i in 1:nrow(G_structure_covariance)) {
        G_structure_covariance[i, 2:4] <- G_structure_covariance[i,2:4] / std_covar[i]  
      }
      
      print(
        round_numeric(G_structure_covariance, 3), 
        row.names = FALSE, 
        ...
      )
      
    } else {
      names(G_structure) <- c("Parameter", "SD", "lower 95% CI", "upper 95% CI") 
      G_structure[, 2:4] <- sqrt(G_structure[, 2:4])
      #cat("Standard deviation\n")
      print(
        round_numeric(G_structure, 3), 
        row.names = FALSE, 
        ...
      )
      
    }
    
    cat("\nR-Structure - Residulas\n\n")
    R_structure <- model_summary$Rcovariances[, -4]
    names(R_structure) <- c("SD", "lower 95% CI", "upper 95% CI") 
    print(round(sqrt(R_structure), 3), ...)
    
  }
 
}
