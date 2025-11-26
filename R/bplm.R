#' Bayesian Piecewise Linear Model
#'
#' Computes a bayesian (hierarchical) piecewise linear model based on a Markov
#' chain Monte Carlo sampler.
#'
#' @inheritParams .inheritParams
#' @order 1
#' @param random_trend If TRUE, includes a random trend effect.
#' @param random_level If TRUE, includes a random level effect.
#' @param random_slope If TRUE, includes a random slope effect.
#' @param fixed A formula that overwrites the automatically created fixed part
#'   of the regression model that defaults to the standard piecewise regression
#'   model. The parameter phase followed by the phase name (e.g., phaseB)
#'   indicates the level effect of the corresponding phase. The parameter
#'   'inter' followed by the phase name (e.g., interB) adresses the slope effect
#'   based on the method provide in the model argument (e.g., "B&L-B"). The
#'   formula can be changed for example to include further L1 or L2 variables
#'   into the regression model.
#' @param random A formula that overwrites the automatically created random part
#'   of the regression model.
#' @param update_fixed An easier way to change the fixed model part (e.g., `. ~
#'   . + newvariable`).
#' @param ... Further arguments passed to the mcmcglmm function.
#' @return An object of class `sc_bplm`.
#'  |  |  |
#'  | --- | --- |
#'  | `model` | List containing information about the applied model. |
#'  | `N` | Number of single-cases. |
#'  | `formula` |A list containing the fixed and the random formulas of the hplm model. |
#'  | `mcmglmm` | Object of class MCMglmm. |
#'  | `contrast` | List with contrast definitions. |
#' @author Juergen Wilbert
#' @family regression functions
#' @examples
#' # plm regression
#' bplm(example_A24)
#' 
#' # Multilevel plm regression with random intercept
#' bplm(exampleAB_50, nitt = 5000)
#' 
#' # Adding a random slope
#' bplm(exampleAB_50, random_level = TRUE, nitt = 5000)
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
  
  attributes(out)[opts("phase", "mt", "dv")] <- list(pvar, mvar, dvar)

  out
}

