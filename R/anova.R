#' ANOVA Table for Piecewise Linear Models
#' 
#' @aliases anova.sc_plm anova.sc_hplm
#' @param object an object containing the results returned by a plm().
#' @param ... additional plm objects.
#' @examples
#' mod1 <- plm(exampleAB$Johanna)
#' mod2 <- plm(exampleAB$Johanna, level = FALSE, slope = FALSE)
#' anova(mod1, mod2)
#' 
#' ## For multilevel models:
#' # mod1 <- hplm(exampleAB_50)
#' # mod2 <- hplm(exampleAB_50, slope = FALSE)
#' # anova(mod1, mod2)
#' @export
anova.sc_plm <- function(object, ...) {
  models <- list(...)
  models <- lapply(models, function(x) x$full.model)
  models <- c(list(object$full.model), models)
  
  do.call(anova, models)
  
}

#' @rdname anova.sc_plm
#' @export
anova.sc_hplm <- function(object, ...) {
  models <- list(...)
  models <- lapply(models, function(x) x$hplm)
  models <- c(list(object$hplm), models)
  
  do.call(anova, models)
  
}
