#' Hierarchical piecewise linear model / piecewise regression
#' 
#' The \code{hplm} function computes a hierarchical piecewise regression model.
#' 
#' @inheritParams .inheritParams
#' @param method Method used to fit your model. Pass \code{"REML"} to maximize
#' the restricted log-likelihood or \code{"ML"} for maximized log-likelihood.
#' Default is \code{"ML"}.
#' @param control A list of settings for the estimation algorithm, replacing
#' the default values passed to the function \code{lmeControl} of the
#' \code{nlme} package.
#' @param random.slopes If \code{random.slopes = TRUE} random slope effects of
#' the level, trend, and treatment parameter are estimated.
#' @param lr.test If set TRUE likelihood ratio tests are calculated comparing model with vs. without random slope parameters.
#' @param ICC If \code{ICC = TRUE} an intraclass-correlation is estimated.
#' @param fixed Defaults to the fixed part of the standard piecewise regression model. The
#' parameter phase followed by the phase name (e.g., phaseB) indicates the level effect of the corresponding phase. The parameter 'inter' followed by the phase name (e.g., interB) adresses the slope effect based on the method
#' provide in the model argument (e.g., "B&L-B"). The formula can be changed
#' for example to include further L1 or L2 variables into the regression model.
#' @param random The random part of the model.
#' @param update.fixed An easier way to change the fixed model part (e.g., . ~ . + newvariable).
#' @param data.l2 A dataframe providing additional variables at Level 2. The
#' scdf File has to have names for all cases and the Level 2 dataframe has to
#' have a column named 'cases' with the names of the cases the Level 2
#' variables belong to.
#' @param ... Further arguments passed to the lme function.
#' @return 
#' \item{model}{List containing infromation about the applied model} 
#' \item{N}{Number of single-cases.}
#' \item{formla}{A list containing the fixed and the random formulas of the hplm model.}
#' \item{hplm}{Object of class lme contaning the multilevel model} 
#' \item{model.0}{Object of class lme containing the Zero Model.} 
#' \item{ICC}{List containing intraclass correlation and test parameters.}
#' \item{model.without}{Object of class gls containing the fixed effect model.}
#' @author Juergen Wilbert
#' @family regression functions
#' @examples
#' 
#' ## Compute hplm model on a MBD over fifty cases (restricted log-likelihood)
#' hplm(exampleAB_50, method = "REML", random.slopes = FALSE)
#' 
#' ## Analyzing with additional L2 variables
#' hplm(Leidig2018, data.l2 = Leidig2018_l2, 
#'      update.fixed = .~. + gender + migration + ITRF_TOTAL*phaseB, 
#'      slope = FALSE, random.slopes = FALSE, lr.test = FALSE)
#'      
#' @export

hplm <- function(data, dvar, pvar, mvar, model = "B&L-B", method = "ML", control = list(opt = "optim"), random.slopes = FALSE, lr.test = FALSE, ICC = TRUE, trend = TRUE, level = TRUE, slope = TRUE, fixed = NULL, random = NULL, update.fixed = NULL, data.l2 = NULL, ...) {

  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- scdf_attr(data, .opt$dv) else scdf_attr(data, .opt$dv) <- dvar
  if (missing(pvar)) pvar <- scdf_attr(data, .opt$phase) else scdf_attr(data, .opt$phase) <- pvar
  if (missing(mvar)) mvar <- scdf_attr(data, .opt$mt) else scdf_attr(data, .opt$mt) <- mvar

  dat <- .SCprepareData(data)
  
  ATTRIBUTES <- attributes(dat)
  
  N <- length(dat)
  out <- list()
  out$model$interaction.method  <- model
  out$model$estimation.method   <- method
  out$model$lr.test             <- lr.test
  out$model$random.slopes       <- random.slopes
  out$model$ICC                 <- ICC
  out$N                         <- N

# interaction and dummy coding and L2 --------------------------------------

  tmp_model <- .add_model_dummies(data = dat, model = model)
  dat <- tmp_model$data

  dat <- longSCDF(dat, l2 = data.l2)

# create formulas ---------------------------------------------------------

  if (is.null(fixed))
    fixed <- as.formula(.create_fixed_formula(
      dvar, mvar, slope, level, trend, tmp_model$VAR_PHASE, tmp_model$VAR_INTER
    ))
  
  if (!is.null(update.fixed)) fixed <- update(fixed, update.fixed)
  
  if (!random.slopes && is.null(random)) random <- as.formula("~1|case")
  
  if (is.null(random))
    random <- as.formula(.create_random_formula(
      mvar, slope, level, trend, tmp_model$VAR_PHASE, tmp_model$VAR_INTER
    ))

  out$formula <- list(fixed = fixed, random = random)
  
# lme hplm model ----------------------------------------------------------

  out$hplm <- lme(
    fixed = fixed, random = random, data = dat, na.action = na.omit, 
    method = method, control = control, keep.data = FALSE, ...
  )
  
  out$hplm$call$fixed <- fixed

# LR tests ----------------------------------------------------------------

  if (lr.test) {
    PREDIC_RAND    <- unlist(strsplit(as.character(random[2]), "\\|"))[1]
    PREDIC_RAND_ID <- unlist(strsplit(as.character(random[2]), "\\|"))[2]
    PREDIC_RAND    <- unlist(strsplit(PREDIC_RAND, "\\+"))
    PREDIC_RAND    <- trimws(PREDIC_RAND)
    PREDIC_RAND_ID <- trimws(PREDIC_RAND_ID)

    if (length(PREDIC_RAND) == 1) {
      stop("LR Test not applicable with only one random effect.")
    }
    
    random.ir <- list(formula(gsub("1", "-1", random)))
    for(i in 2:length(PREDIC_RAND))
      random.ir[[i]] <- formula(
        paste0("~", paste0(PREDIC_RAND[!PREDIC_RAND %in% PREDIC_RAND[i]], 
        collapse = " + "), " | ", PREDIC_RAND_ID)
      )
    
    out$random.ir$restricted <- list()
    
    # lme
    for(i in 1:length(random.ir)) {
      out$random.ir$restricted[[i]] <- lme(
        fixed = fixed, random = random.ir[i], data = dat, 
        na.action = na.omit, method = method, control=control, 
        keep.data = FALSE, ...)
      
      out$random.ir$restricted[[i]]$call$fixed <- fixed
    }
    out$LR.test <- list()
    
    # LR test
    for(i in 1:length(random.ir)) {
      out$LR.test[[i]] <- anova(out$random.ir$restricted[[i]], out$hplm)
      
    }
    attr(out$random.ir, "parameters") <- c("Intercept", PREDIC_RAND)
  }
  
  if (ICC) {
    .formula.null <- as.formula(paste0(dvar, " ~ 1"))
    out$model.0 <- lme(
      .formula.null, random =~1|case, data = dat, 
      method = method, na.action=na.omit, control = control
    )
    out$model.0$call$fixed <- .formula.null
    
    VC <- as.numeric(VarCorr(out$model.0))
    out$ICC$value <- VC[1] / (VC[1] + VC[2])	
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
  
  class(out) <- c("sc","hplm")
  attr(out, .opt$phase) <- pvar
  attr(out, .opt$mt)    <- mvar
  attr(out, .opt$dv)    <- dvar
  
  out
}

