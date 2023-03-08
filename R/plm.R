#' Piecewise linear model / piecewise regression
#'
#' The `plm` function computes a piecewise regression model (see Huitema &
#' McKean, 2000).
#'
#' @inheritParams .inheritParams
#' @param AR Maximal lag of autoregression. Modeled based on the
#'   Autoregressive-Moving Average (ARMA) function.  When AR is set, the family
#'   argument must be set to `family = "gaussian"`.
#' @param family Set the distribution family. Defaults to a gaussian
#'   distribution. See the `family` function for more details.
#' @param formula Defaults to the standard piecewise regression model. The
#'   parameter phase followed by the phase name (e.g., phaseB) indicates the
#'   level effect of the corresponding phase. The parameter 'inter' followed by
#'   the phase name (e.g., interB) adresses the slope effect based on the method
#'   provide in the model argument (e.g., "B&L-B"). The formula can be changed
#'   for example to include further variables into the regression model.
#' @param update An easier way to change the regression formula (e.g., `. ~ . +
#'   newvariable`).
#' @param na.action Defines how to deal with missing values.
#' @param r_squared Logical. If TRUE, delta r_squares will be calculated for
#'   each predictor.
#' @param var_trials Name of the variable containing the number of trials (only
#'   for binomial regressions). If a single integer is provided this is
#'   considered to be a the constant number of trials across all measurements.
#' @param dvar_percentage Only for binomial distribution. If set TRUE, the
#'   dependent variable is assumed to represent proportions `[0,1]`. Otherwise
#'   dvar is assumed to represent counts.
#' @param ... Further arguments passed to the glm function.
#' @return
#' \item{formula}{plm formula. Uselful if you want to use the update or
#'   formula argument and you don't know the names of the parameters.}
#' \item{model}{Character string from function call (see `Arguments`
#'   above).}
#' \item{F.test}{F-test values of modelfit.} \item{r.squares}{Explained variance
#' R squared for each model parameter.}
#' \item{ar}{Autoregression lag from function call (see `Arguments`
#'   above).}
#' \item{family}{Distribution family from function call
#'   (see `Arguments` above).}
#' \item{full.model}{Full regression model list from the gls or glm function.}
#' @author Juergen Wilbert
#' @family regression functions
#' @references Beretvas, S., & Chung, H. (2008). An evaluation of modified
#'   R2-change effect size indices for single-subject experimental designs.
#'   *Evidence-Based Communication Assessment and Intervention, 2*,
#'   120-128.
#'
#'   Huitema, B. E., & McKean, J. W. (2000). Design specification issues in
#'   time-series intervention models. *Educational and Psychological
#'   Measurement, 60*, 38-58.
#' @examples
#'
#' ## Compute a piecewise regression model for a random single-case
#' set.seed(123)
#' AB <- design(
#'   phase_design = list(A = 10, B = 20),
#'   level = list(A = 0, B = 1), slope = list(A = 0, B = 0.05),
#'   trend = 0.05
#' )
#' dat <- random_scdf(design = AB)
#' plm(dat, AR = 3)
#'
#' ## Another example with a more complex design
#' A1B1A2B2 <- design(
#'   phase_design = list(A1 = 15, B1 = 20, A2 = 15, B2 = 20),
#'   level = list(A1 = 0, B1 = 1, A2 = -1, B2 = 1),
#'   slope = list(A1 = 0, B1 = 0.0, A2 = 0, B2 = 0.0),
#'   trend = 0.0)
#' dat <- random_scdf(design = A1B1A2B2, seed = 123)
#' plm(dat, contrast = "preceding")
#'
#' ## no slope effects were found. Therefore, you might want to the drop slope
#' ## estimation:
#' plm(dat, slope = FALSE, contrast = "preceding")
#'
#' ## and now drop the trend estimation as well
#' plm(dat, slope = FALSE, trend = FALSE, contrast = "preceding")
#'
#' ## A poisson regression
#' example_A24 %>%
#'   plm(family = "poisson")
#'
#' ## A binomial regression (frequencies as dependent variable)
#' plm(exampleAB_score$Christiano, family = "binomial", var_trials = "trials")
#'
#' ## A binomial regression (percentage as dependent variable)
#' exampleAB_score$Christiano %>%
#'   transform(percentage = values/trials) %>%
#'   set_dvar("percentage") %>%
#'   plm(family = "binomial", var_trials = "trials", dvar_percentage = TRUE)
#' @export
plm <- function(data, dvar, pvar, mvar, 
                AR = 0, 
                model = c("W", "H-M", "B&L-B", "JW"),
                family = "gaussian", 
                trend = TRUE, 
                level = TRUE, 
                slope = TRUE,
                contrast = c("first", "preceding"),
                contrast_level = c(NA, "first", "preceding"),
                contrast_slope = c(NA, "first", "preceding"),
                formula = NULL, 
                update = NULL, 
                na.action = na.omit,
                r_squared = TRUE,
                var_trials = NULL,
                dvar_percentage = FALSE,
                ...) {
  
  check_args(
    has_length(data, 1, 
               "plm can not be applied to more than one case (use hplm)."),
    not(family != "gaussian" && AR != 0, 
        "family is not 'gaussian' but AR is set."),
    not(family == "binomial" && is.null(var_trials),
        "family is 'binomial' but 'var_trials' is not defined."),
    by_call(model, "plm"),
    by_call(contrast_level, "plm"),
    by_call(contrast_slope, "plm"),
    by_call(contrast, "plm")
  )
  
  model <- model[1]
  contrast <- contrast[1]
  contrast_level <- contrast_level[1]
  contrast_slope <- contrast_slope[1]
  
  # set defaults attributes
  if (missing(dvar)) dvar <- dv(data) else dv(data) <- dvar
  if (missing(pvar)) pvar <- phase(data) else phase(data) <- pvar
  if (missing(mvar)) mvar <- mt(data) else mt(data) <- mvar

  data <- .prepare_scdf(data, na.rm = TRUE)

  if (is.na(contrast_level)) contrast_level <- contrast
  if (is.na(contrast_slope)) contrast_slope <- contrast
  
  if (model == "JW") {
    contrast_level <- "preceding"
    contrast_slope <- "preceding"
    model <- "B&L-B"
  }
  
  if (family != "gaussian") r_squared = FALSE
  
  original_attr <- attributes(data)[[opt("scdf")]]
  
  # formula definition ------------------------------------------------------
  
  tmp_model <- .add_model_dummies(
    data = data, model = model, 
    contrast_level = contrast_level, contrast_slope = contrast_slope
  )

  data  <- tmp_model$data[[1]]
  
  if(is.null(formula)) {
    tmp <- .create_fixed_formula(
      dvar, mvar, slope, level, trend, 
      tmp_model$var_phase, tmp_model$var_inter
    ) 
    formula <- as.formula(tmp)
  } 
  
  if(!is.null(update)) formula <- update(formula, update)
  
  predictors <- as.character(formula[3])
  predictors <- unlist(strsplit(predictors, "\\+"))
  predictors <- trimws(predictors)
  if(!is.na(match("1", predictors)))
    predictors <- predictors[-match("1", predictors)]
  
  formula_full <- formula
  formulas_restricted  <- sapply(
    predictors, function(x) update(formula, formula(paste0(".~. - ", x)))
  )
  
  # glm models --------------------------------------------------------------
  
  if(AR == 0) {
    
    if (family != "binomial") {
      full <- glm(
        formula_full, data = data, family = family, na.action = na.action, ...
      )
    }
    
    if (family == "binomial") {
      trials <- data[[var_trials]]  
      if (!dvar_percentage) data[[dvar]] <- data[[dvar]] / trials
      full <- glm(
        formula_full, data = data, family = family, na.action = na.action, 
        weights = trials, ...
      )
    }
    
    df_residuals <- full$df.residual
    df_intercept <- if (attr(full$terms, "intercept")) 1 else 0
    
    if (r_squared) {
      restricted.models <- lapply(
        formulas_restricted, 
        function(x) 
          glm(x, data = data, family = family, na.action = na.action, ...)
      )
    }
  }
  
  if(AR > 0) {
    full <- gls(
      formula_full, data = data, correlation = corARMA(p = AR), 
      method = "ML", na.action = na.action
    )
    df_residuals <- full$dims$N - full$dims$p
    df_intercept <- if ("(Intercept)" %in% names(full$parAssign)) 1 else 0
    
    if (r_squared) {
      restricted.models <- lapply(
        formulas_restricted, 
        function(x) 
          gls(model = x, data = data, correlation = corARMA(p = AR), 
              method = "ML", na.action = na.action
          )
      )
    }
    
  }
  
  # F and R-Squared ---------------------------------------------------------
  
  if (family == "gaussian") {
    n <- length(full$residuals)
    df_effect <- n - 1 - df_residuals
    
    QSE <- sum(full$residuals^2, na.rm = TRUE)
    QST <- sum((data[[dvar]] - mean(data[[dvar]]))^2)
    MQSA <- (QST - QSE) / df_effect
    MQSE <- QSE / df_residuals
    F <- MQSA / MQSE
    p <- pf(F, df_effect, df_residuals, lower.tail = FALSE)
    
    total_variance <- var(data[[dvar]])
    r2 <- 1 - (var(full$residuals) / total_variance)
    r2_adj <- 1 - (1 - r2) * ((n - df_intercept) / df_residuals)
    
    if (r_squared) {
      r_squares <- lapply(restricted.models, function(x) 
        r2 - (1 - (var(x$residuals, na.rm = TRUE) / total_variance))
      )
      r_squares <- unlist(r_squares)
    } else r_squares <- NA
    
    F_test <- c(
      F = F, df1 = df_effect, df2 = df_residuals, p = p, 
      R2 = r2, R2.adj = r2_adj
    )
  }
  
  if (family != "gaussian") {
    F_test <- NA
    r_squares <- NA
  }
  
  
  
  # output ------------------------------------------------------------------
  
  out <- list(
    formula = formula_full, 
    model = model, 
    contrast = list(level = contrast_level, slope = contrast_slope),
    F.test = F_test, 
    r.squares = r_squares, 
    ar = AR, 
    family = family, 
    var_trials = var_trials,
    dvar_percentage = dvar_percentage,
    full.model = full, 
    data = data
  )
  
  class(out) <- c("sc_plm")
  attr(out, opt("phase"))  <- original_attr[opt("phase")]
  attr(out, opt("mt"))     <- original_attr[opt("mt")]
  attr(out, opt("dv"))     <- original_attr[opt("dv")]
  out
}
