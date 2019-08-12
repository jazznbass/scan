#' Multivariate Piecewise linear model / piecewise regression
#'
#' This function is in an experimental status.
#' The \code{mplm} function computes a multivariate piecewise regression model.
#'
#'
#' @inheritParams .inheritParams
#' @param formula Defaults to the standard piecewise regression model. The
#' parameter phase followed by the phase name (e.g., phaseB) indicates the level effect of the corresponding phase. The parameter 'inter' followed by the phase name (e.g., interB) adresses the slope effect based on the method
#' provide in the model argument (e.g., "B&L-B"). The formula can be changed
#' for example to include further variables into the regression model.
#' @param update An easier way to change the regression formula (e.g., . ~ . + newvariable).
#' @param na.action Defines how to deal with missing values
#' @param ... Further arguments passed to the lm function.
#' @return \item{model}{Character string from function call (see
#' \code{Arguments} above).}
#' \item{full.model}{Full regression model list}
#' @author Juergen Wilbert
#' @family regression functions
#' @examples
#' ##
#' mplm(exampleAB_add, dvar = c("wellbeing", "depression"))
#' @export

mplm <- function(data, dvar, mvar, pvar, model = "B&L-B", trend = TRUE, 
                 level = TRUE, slope = TRUE, formula = NULL, update = NULL, 
                 na.action = na.omit, ...) {
  cat(.opt$function_debugging_warning)
  if (!requireNamespace("car", quietly = TRUE)) {
    stop("Package car needed for this function to work. Please install it.",
      call. = FALSE
    )
  }
  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- scdf_attr(data, .opt$dv) else scdf_attr(data, .opt$dv) <- dvar
  if (missing(pvar)) pvar <- scdf_attr(data, .opt$phase) else scdf_attr(data, .opt$phase) <- pvar
  if (missing(mvar)) mvar <- scdf_attr(data, .opt$mt) else scdf_attr(data, .opt$mt) <- mvar

  data <- .SCprepareData(data)

  N <- length(data)
  if (N > 1) {
    stop("Procedure could not be applied to more than one case.\n")
  }


  ### model definition
  tmp_model <- .add_model_dummies(data = data, model = model)
  data <- tmp_model$data[[1]]

  if (is.null(formula)) {
    formula <- as.formula(.create_fixed_formula(
      dvar = "y", mvar = mvar, slope = slope, level = level,
      trend = trend, VAR_PHASE = tmp_model$VAR_PHASE, VAR_INTER = tmp_model$VAR_INTER
    ))
  }

  if (!is.null(update)) formula <- update(formula, update)

  PREDICTORS <- as.character(formula[3])
  PREDICTORS <- unlist(strsplit(PREDICTORS, "\\+"))
  PREDICTORS <- trimws(PREDICTORS)
  if (!is.na(match("1", PREDICTORS))) {
    PREDICTORS <- PREDICTORS[-match("1", PREDICTORS)]
  }

  y <- as.matrix(data[, dvar])

  full <- lm(formula, data = data, na.action = na.action, ...)
  out <- list(model = model, full.model = full, formula = formula)

  class(out) <- c("sc", "mpr")
  out
}
