#' Hierarchical piecewise linear model / piecewise regression
#'
#' The [hplm()] function computes a hierarchical piecewise regression model.
#'
#' @inheritParams .inheritParams
#' @order 1
#' @param method Method used to fit your model. Pass `"REML"` to maximize the
#'   restricted log-likelihood or `"ML"` for maximized log-likelihood. Default
#'   is `"ML"`.
#' @param control A list of settings for the estimation algorithm, replacing the
#'   default values passed to the function `lmeControl` of the `nlme` package.
#' @param random.slopes If `random.slopes = TRUE` random slope effects of the
#'   level, trend, and treatment parameter are estimated.
#' @param lr.test If set TRUE likelihood ratio tests are calculated comparing
#'   model with vs. without random slope parameters.
#' @param ICC If `ICC = TRUE` an intraclass-correlation is estimated.
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
#' @param update.fixed An easier way to change the fixed model part
#'   (e.g., `. ~ . + newvariable`).
#' @param data.l2 A dataframe providing additional variables at Level 2. The
#'   scdf File has to have names for all cases and the Level 2 dataframe has to
#'   have a column named 'cases' with the names of the cases the Level 2
#'   variables belong to.
#' @param ... Further arguments passed to the lme function.
#' @return \item{model}{List containing infromation about the applied model}
#' \item{N}{Number of single-cases.} \item{formla}{A list containing the fixed
#' and the random formulas of the hplm model.} \item{hplm}{Object of class lme
#' contaning the multilevel model} \item{model.0}{Object of class lme containing
#' the Zero Model.} \item{ICC}{List containing intraclass correlation and test
#' parameters.} \item{model.without}{Object of class gls containing the fixed
#' effect model.}
#' @author Juergen Wilbert
#' @family regression functions
#' @examples
#'
#' ## Compute hplm model on a MBD over fifty cases (restricted log-likelihood)
#' hplm(exampleAB_50, method = "REML", random.slopes = FALSE)
#'
#' ## Analyzing with additional L2 variables
#' Leidig2018 |>
#'   add_l2(Leidig2018_l2) |>
#'   hplm(update.fixed = .~. + gender + migration + ITRF_TOTAL*phaseB,
#'        slope = FALSE, random.slopes = FALSE, lr.test = FALSE
#'   )
#'
#' @export

lme4_hplm <- function(data, dvar, pvar, mvar, 
                 model = c("W", "H-M", "B&L-B", "JW"),
                 contrast = c("first", "preceding"),
                 contrast_level = NA,
                 contrast_slope = NA,
                 method = c("ML", "REML"), 
                 control = lmerControl(), 
                 random.slopes = FALSE, 
                 lr.test = FALSE, 
                 ICC = TRUE, 
                 trend = TRUE, 
                 level = TRUE, 
                 slope = TRUE, 
                 random_trend = FALSE, 
                 random_level = FALSE, 
                 random_slope = FALSE, 
                 fixed = NULL, 
                 random = NULL, 
                 update.fixed = NULL, 
                 data.l2 = NULL, 
                 ...) {

  check_args(
    
    by_call(model),
    by_call(method),
    by_call(contrast)
  )
  model <- model[1]
  method <- method[1]
  contrast <- contrast[1]
  
  if (is.na(contrast_level)) contrast_level <- contrast
  if (is.na(contrast_slope)) contrast_slope <- contrast
  
  if (model == "JW") {
    contrast_level <- "preceding"
    contrast_slope <- "preceding"
    model <- "B&L-B"
  }

  if (random.slopes) {
    random_trend <- trend
    random_level <- level
    random_slope <- slope
  }
  
  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- dv(data) else dv(data) <- dvar
  if (missing(pvar)) pvar <- phase(data) else phase(data) <- pvar
  if (missing(mvar)) mvar <- mt(data) else mt(data) <- mvar
  
  dat <- .prepare_scdf(data)
  
  N <- length(dat)
  out <- list()
  out$model$interaction.method  <- model
  out$model$contrast.method     <- contrast
  out$model$estimation.method   <- method
  out$model$lr.test             <- lr.test
  out$model$random.slopes       <- random.slopes
  out$model$ICC                 <- ICC
  out$N                         <- N

# interaction and dummy coding and L2 --------------------------------------

  tmp_model <- .add_model_dummies(
    data = dat, model = model, 
    contrast_level = contrast_level, contrast_slope = contrast_slope
  )
  dat <- as.data.frame(tmp_model$data, l2 = data.l2)

# create formulas ---------------------------------------------------------
  
  if (is.null(fixed)) {
    fixed <- .create_fixed_formula(
      dvar, mvar, slope, level, trend, tmp_model$var_phase, tmp_model$var_inter
    )
  }
  #if (!is.null(update.fixed)) fixed <- update(fixed, update.fixed)
  
  
  if (any(random_trend, random_level, random_slope)) {
    random.slopes <- TRUE
  }
  
  if (!random.slopes && is.null(random)) random <- "(1|case)"
  
  if (is.null(random)) {
    random <-.create_random_formula_lmer(
      mvar, 
      random_slope, random_level, random_trend, 
      tmp_model$var_phase, tmp_model$var_inter
    )
  }
  
  hplm_formula <- as.formula(paste0(fixed, " + ", random))
  
                             
  out$formula <- list(fixed = fixed, random = random)
  
# lme hplm model ----------------------------------------------------------

  out$hplm <- lmer(
    formula = hplm_formula, 
    data = dat, 
    na.action = na.omit, 
    REML = if (method == "REML") TRUE else FALSE,
    control = control,
    ...
  )
  browser()
  out$hplm$call$fixed <- fixed

# LR tests ----------------------------------------------------------------

  if (lr.test) {
    pred_rand    <- unlist(strsplit(as.character(random[2]), "\\|"))[1]
    pred_rand_id <- unlist(strsplit(as.character(random[2]), "\\|"))[2]
    pred_rand    <- unlist(strsplit(pred_rand, "\\+"))
    pred_rand    <- trimws(pred_rand)
    pred_rand_id <- trimws(pred_rand_id)

    if (length(pred_rand) == 1) {
      stop("LR Test not applicable with only one random effect.")
    }
    
    random_ir <- list(formula(gsub("1", "-1", paste(random, collapse = " "))))
    for(i in 2:length(pred_rand)) {
      random_ir[[i]] <- formula(
        paste0("~", paste0(pred_rand[!pred_rand %in% pred_rand[i]], 
        collapse = " + "), " | ", pred_rand_id)
      )
    }
    out$random_ir$restricted <- list()
    
    # lme
    for(i in 1:length(random_ir)) {
      out$random_ir$restricted[[i]] <- lme(
        fixed = fixed, random = random_ir[i], data = dat, 
        na.action = na.omit, method = method, control=control, 
        keep.data = FALSE, ...)
      
      out$random_ir$restricted[[i]]$call$fixed <- fixed
    }
    out$LR.test <- list()
    
    # LR test
    for(i in 1:length(random_ir)) {
      out$LR.test[[i]] <- anova(out$random_ir$restricted[[i]], out$hplm)
    }
    attr(out$random_ir, "parameters") <- c("Intercept", pred_rand)
  }
  
  if (ICC) {
    .formula.null <- as.formula(paste0(dvar, " ~ 1"))
    out$model.0 <- lme(
      .formula.null, random =~1|case, data = dat, 
      method = method, na.action=na.omit, control = control
    )
    out$model.0$call$fixed <- .formula.null
    
    vc <- as.numeric(VarCorr(out$model.0))
    out$ICC$value <- vc[1] / (vc[1] + vc[2])	
    out$model.without <- gls(
      .formula.null, data = dat, method = method, 
      na.action = na.omit, control = control
    )
    out$model.without$call$model <- .formula.null
    dif <- anova(out$model.0, out$model.without)
    out$ICC$L <- dif$L.Ratio[2]
    out$ICC$p <- dif$"p-value"[2]
  } 
  
  out$model$fixed  <- fixed
  out$model$random <- random
  out$contrast <- list(level = contrast_level, slope = contrast_slope)
  
  class(out) <- c("sc_hplm")
  attr(out, opt("phase")) <- pvar
  attr(out, opt("mt"))    <- mvar
  attr(out, opt("dv"))    <- dvar
  
  out
}


.create_random_formula_lmer <- function(mvar, 
                                        slope, level, trend, 
                                        var_phase, var_inter) {
  inter <- ""
  phase <- ""
  mt    <- ""
  if (slope) {
    inter <- paste0(var_inter, collapse = "+")
    inter <- paste0("+ ", inter)
  }
  if (level) {
    phase <- paste0(var_phase, collapse = "+")
    phase <- paste0("+ ", phase)
  }
  if (trend)
    mt <- paste0("+ ", mvar, " ")
  paste0("(1", mt, phase, inter, "|case)")
}

