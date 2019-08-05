#' Piecewise linear model / piecewise regression
#' 
#' The \code{plm} function computes a piecewise regression model (see Huitema &
#' McKean, 2000).
#' 
#' 
#' @inheritParams .inheritParams
#' @param AR Maximal lag of autoregression. Modeled based on the
#' Autoregressive-Moving Average (ARMA) function.  When AR is set, the family
#' argument must be set to \code{family = "gaussian"}.
#' @param family Set the distributioin family. Defaults to a gaussian
#' distribution. See the \code{family} function for more details.
#' @param formula Defaults to the standard piecewise regression model. The
#' parameter phase followed by the phase name (e.g., phaseB) indicates the level effect of the corresponding phase. The parameter 'inter' followed by the phase name (e.g., interB) adresses the slope effect based on the method
#' provide in the model argument (e.g., "B&L-B"). The formula can be changed
#' for example to include further variables into the regression model.
#' @param update An easier way to change the regression formula (e.g., . ~ . + newvariable).
#' @param na.action Defines how to deal with missing values
#' @param ... Further arguments passed to the glm function.
#' @return 
#' \item{formula}{plm formula. Uselful if you want to use the update or formula argument and you don't know the names of the parameters.}
#' \item{model}{Character string from function call (see \code{Arguments} above).} 
#' \item{F.test}{F-test values of modelfit.}
#' \item{r.squares}{Explained variance R squared for each model parameter.}
#' \item{ar}{Autoregression lag from function call (see \code{Arguments} above).}
#' \item{family}{Distribution family from function call (see \code{Arguments} above).}
#' \item{full.model}{Full regression model list from the gls or glm function.}
#' @author Juergen Wilbert
#' @family regression functions
#' @references Beretvas, S., & Chung, H. (2008). An evaluation of modified
#' R2-change effect size indices for single-subject experimental designs.
#' \emph{Evidence-Based Communication Assessment and Intervention, 2}, 120-128.
#' 
#' Huitema, B. E., & McKean, J. W. (2000). Design specification issues in
#' time-series intervention models. \emph{Educational and Psychological
#' Measurement, 60}, 38-58.
#' @examples
#' 
#' ## Compute a piecewise regression model for a random single-case
#' set.seed(123)
#' AB <- design_rSC(
#'   phase.design = list(A = 10, B = 20), 
#'   level = list(A = 0, B = 1), slope = list(A = 0, B = 0.05), 
#'   trend = list(0.05)
#' )
#' dat <- rSC(design = AB)
#' plm(dat, AR = 3)
#' 
#' ## Another example with a more complex design
#' A1B1A2B2 <- design_rSC(
#'   phase.design = list(A1 = 15, B1 = 20, A2 = 15, B2 = 20), 
#'   level = list(A1 = 0, B1 = 1, A2 = -1, B2 = 1),
#'   slope = list(A1 = 0, B1 = 0.0, A1 = 0, B2 = 0.0),
#'   trend = list(0.0))
#' dat <- rSC(design = A1B1A2B2, seed = 123)
#' plm(dat, model = "JW")
#' 
#' ## no slope effects were found. Therefore you might want to drop slope estimation:
#' plm(dat, slope = FALSE, model = "JW")
#' 
#' ## and now drop the trend estimation as well
#' plm(dat, slope = FALSE, trend = FALSE, model = "JW")
#' 
#' @export
plm <- function(data, dvar, pvar, mvar, AR = 0, model = "B&L-B", family = "gaussian", trend = TRUE, level = TRUE, slope = TRUE,formula = NULL, update = NULL, na.action = na.omit, ...) {

  if (AR > 0 && !family == "gaussian") {
    stop("Autoregression models could only be applied if distribution familiy = 'gaussian'.\n")
  }
  
  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- scdf_attr(data, .opt$dv) else scdf_attr(data, .opt$dv) <- dvar
  if (missing(pvar)) pvar <- scdf_attr(data, .opt$phase) else scdf_attr(data, .opt$phase) <- pvar
  if (missing(mvar)) mvar <- scdf_attr(data, .opt$mt) else scdf_attr(data, .opt$mt) <- mvar
  
  data <- .SCprepareData(data, na.rm = TRUE)
  
  ATTRIBUTES <- attributes(data)[[.opt$scdf]]
  
  N <- length(data)
  if(N > 1) {
    stop("Procedure could not be applied to more than one case.\n
         Consider to use the hplm function.")
  }

# formula definition ------------------------------------------------------
  
  tmp_model <- .add_model_dummies(data = data, model = model)
  data      <- tmp_model$data[[1]]

  if(is.null(formula)) {
      formula <- as.formula(.create_fixed_formula(
        dvar, mvar, slope, level, trend, tmp_model$VAR_PHASE, tmp_model$VAR_INTER
        ))
  } 
  
  if(!is.null(update)) formula <- update(formula, update)
  
  PREDICTORS <- as.character(formula[3])
  PREDICTORS <- unlist(strsplit(PREDICTORS, "\\+"))
  PREDICTORS <- trimws(PREDICTORS)
  if(!is.na(match("1", PREDICTORS)))
     PREDICTORS <- PREDICTORS[-match("1", PREDICTORS)]
  
  formula.full <- formula
  formulas.ir  <- sapply(PREDICTORS, function(x) update(formula, formula(paste0(".~. - ", x))))

# glm models --------------------------------------------------------------
  
  if(AR == 0) {
    full <- glm(formula.full, data = data, family = family, na.action = na.action, ...)
    restricted.models <- lapply(formulas.ir, function(x) glm(x, data = data, family = family, na.action = na.action, ...))
    df2.full <- full$df.residual
    df.int <- if (attr(full$terms, "intercept")) 1 else 0
  }

  if(AR > 0) {
    full <- gls(formula.full, data = data, correlation = corARMA(p = AR), method = "ML", na.action = na.action)
    restricted.models <- 
      lapply(formulas.ir, function(x) 
        gls(model = x, data = data, correlation = corARMA(p = AR), method = "ML", na.action = na.action)
      )
    df2.full <- full$dims$N - full$dims$p
    df.int <- if ("(Intercept)" %in% names(full$parAssign)) 1 else 0
  }

# F and R-Squared ---------------------------------------------------------

  n <- length(full$residuals)
  df1.full <- n - 1 - df2.full
  
  QSE <- sum(full$residuals^2, na.rm = TRUE)
  QST <- sum((data[, dvar] - mean(data[, dvar]))^2)
  MQSA <- (QST - QSE) / df1.full
  MQSE <- QSE / df2.full
  F.full <- MQSA / MQSE
  p.full <- pf(F.full, df1.full, df2.full, lower.tail = FALSE)
  
  total.variance <- var(data[, dvar])
  r2.full     <- 1 - (var(full$residuals) / total.variance)
  r2.full.adj <- 1 - (1 - r2.full) * ((n - df.int) / df2.full)

  r.squares <- lapply(restricted.models, function(x) 
    r2.full - (1 - (var(x$residuals, na.rm = TRUE) / total.variance))
  )
  r.squares <- unlist(r.squares)
  
# output ------------------------------------------------------------------

  F.test <- c(
    F = F.full, df1 = df1.full, df2 = df2.full, p = p.full, 
    R2 = r2.full, R2.adj = r2.full.adj
  )
  
  out <- list(
    formula = formula.full, model = model, F.test = F.test, 
    r.squares = r.squares, ar = AR, family = family, full.model = full, 
    data = data
  )

  class(out) <- c("sc", "pr")
  attr(out, .opt$phase)  <- ATTRIBUTES[.opt$phase]
  attr(out, .opt$mt)     <- ATTRIBUTES[.opt$mt]
  attr(out, .opt$dv)     <- ATTRIBUTES[.opt$dv]
  out
}
