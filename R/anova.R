#' ANOVA Table for Piecewise Linear Models
#' 
#' Model comparison for piecewise regression models fitted with `plm()`,
#' `hplm()`, or `mplm()` using likelihood ratio tests.
#' 
#' @aliases anova.sc_plm anova.sc_hplm anova.sc_mplm
#' @param object An object containing the results returned by `plm()`,
#' `hplm()`, or `mplm()`.
#' @param ... additional objects for model comparison.
#' @return An object of class `anova` containing the results of the model
#'  comparison.
#' @details
#' The function performs likelihood ratio tests to compare nested piecewise
#' regression models. It extracts the underlying model from the sc_plm, sc_hplm,
#' or sc_mplm object and passes them to the generic anova() function
#' for model comparison.
#'  
#' @examples
#' ## For glm models with family = "gaussian"
#' mod1 <- plm(exampleAB$Johanna, level = FALSE, slope = FALSE)
#' mod2 <- plm(exampleAB$Johanna)
#' anova(mod1, mod2)
#' ## For glm models with family = "poisson"
#' mod0 <- plm(example_A24, formula = injuries ~ 1, family = "poisson")
#' mod1 <- plm(example_A24, trend = FALSE, family = "poisson")
#' anova(mod0, mod1)
#' ## For glm with family = "binomial"
#' mod0 <- plm(
#'   exampleAB_score$Christiano, 
#'   formula = values ~ 1, 
#'   family = "binomial", 
#'   var_trials = "trials"
#' )
#' mod1 <- plm(
#'   exampleAB_score$Christiano, 
#'   trend = FALSE, 
#'   family = "binomial", 
#'   var_trials = "trials"
#' )
#' anova(mod0, mod1)
#' @export
anova.sc_plm <- function(object, ...) {
  models <- list(...)
  models <- lapply(models, function(x) {
    if (inherits(x, "sc_plm")) x$full.model else x
  })
  models <- c(list(object$full.model), models)
  
  do.call(anova, models)
}

#' @rdname anova.sc_plm
#' @examples
#' ## For multilevel models:
#' mod0 <- hplm(Leidig2018, trend = FALSE, slope = FALSE, level = FALSE)
#' mod1 <- hplm(Leidig2018, trend = FALSE)
#' mod2 <- hplm(Leidig2018)
#' anova(mod0, mod1, mod2)
#' @export
anova.sc_hplm <- function(object, ...) {
  models <- list(...)
  models <- lapply(models, function(x) {
    if (inherits(x, "sc_hplm")) x$hplm else x
  })

  models <- c(list(object$hplm), models)

  is_model <- vapply(models, inherits, logical(1), what = "lme")
  
  arg_names <- names(models)
  if (is.null(arg_names)) arg_names <- rep("", length(models))
  
  args <- c(
    lapply(which(is_model), function(i) str2lang(paste0("models[[", i, "]]"))),
    models[!is_model]
  )
  names(args) <- c(rep("", sum(is_model)), arg_names[!is_model])
  
  out <- eval(as.call(c(list(quote(anova)), args)))
  row.names(out) <- NULL
  out
}

#' @rdname anova.sc_plm
#' @examples
#' ## For mplm
#' mod0 <- mplm(
#'   Leidig2018$`1a1`, 
#'   update = . ~  1, dvar = c("academic_engagement", "disruptive_behavior")
#' )
#' mod1 <- mplm(
#'   Leidig2018$`1a1`, 
#'   trend = FALSE, 
#'   dvar = c("academic_engagement", "disruptive_behavior")
#' )
#' mod2 <- mplm(
#'   Leidig2018$`1a1`, 
#'   dvar = c("academic_engagement", "disruptive_behavior")
#' )
#' 
#' anova(mod0, mod1, mod2)
#' @export
anova.sc_mplm <- function(object, ...) {
  models <- list(...)
  models <- lapply(models, function(x) {
    if (inherits(x, "sc_mplm")) x$full.model else x
  })
  models <- c(list(object$full.model), models)
  
  do.call(anova, models)
  
}
