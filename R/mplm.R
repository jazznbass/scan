#' Multivariate Piecewise linear model / piecewise regression
#'
#' The [mplm()] function computes a multivariate piecewise regression model.
#'
#' @inheritParams .inheritParams
#' @order 1
#' @param formula Defaults to the standard piecewise regression model. The
#'   parameter phase followed by the phase name (e.g., `phaseB`) indicates the
#'   level effect of the corresponding phase. The parameter 'inter' followed by
#'   the phase name (e.g., `interB`) adresses the slope effect based on the
#'   method provide in the model argument (e.g., `"B&L-B"`). The formula can be
#'   changed for example to include further variables into the regression model.
#' @param update An easier way to change the regression formula (e.g., `. ~ . +
#'   newvariable`).
#' @param na.action Defines how to deal with missing values.
#' @param ... Further arguments passed to the [lm()] function.
#' @return 
#'  |  |  |
#'  | --- | --- |
#'  | `model` | Character string from function call (see arguments above). |
#'  | `contrast` | List with contrast definitions. |
#'  | `full.model` | Full regression model list. |
#'  | `formula` | 	Formula of the mplm model. | 
#' @author Juergen Wilbert
#' @family regression functions
#' @examples
#' res <- mplm(Leidig2018$`1a1`,
#'   dvar = c("academic_engagement", "disruptive_behavior")
#' )
#' print(res)
#' ## also report standardized coefficients:
#' print(res, std = TRUE)
#' @export

mplm <- function(data, dvar, mvar, pvar, 
                 model = c("W", "H-M", "B&L-B", "JW"),
                 contrast = c("first", "preceding"),
                 contrast_level = c(NA, "first", "preceding"),
                 contrast_slope = c(NA, "first", "preceding"),
                 trend = TRUE, 
                 level = TRUE, 
                 slope = TRUE, 
                 formula = NULL, 
                 update = NULL, 
                 na.action = na.omit, 
                 ...) {
 
  check_args(
    by_call(model),
    by_call(contrast),
    by_call(contrast_level),
    by_call(contrast_slope)
  )
  model <- model[1]
  contrast <- contrast[1]
  contrast_level <- contrast_level[1]
  contrast_slope <- contrast_slope[1]
  
  if (model == "JW") {
    contrast_level <- "preceding"
    contrast_slope <- "preceding"
    model <- "B&L-B"
  }
  
  if (is.na(contrast_level)) contrast_level <- contrast
  if (is.na(contrast_slope)) contrast_slope <- contrast
  
  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- dv(data) else dv(data) <- dvar
  if (missing(pvar)) pvar <- phase(data) else phase(data) <- pvar
  if (missing(mvar)) mvar <- mt(data) else mt(data) <- mvar

  data <- .prepare_scdf(data)

  N <- length(data)
  if (N > 1) {
    stop("Procedure could not be applied to more than one case.\n")
  }

  ### model definition
  tmp_model <- .add_dummy_variables(
    data = data, 
    model = model, 
    contrast_level = contrast_level, 
    contrast_slope = contrast_slope
  )
  
  data <- tmp_model$data[[1]]

  if (is.null(formula)) {
    formula <- .create_fixed_formula(
      dvar = "y", 
      mvar = mvar, 
      slope = slope, 
      level = level, 
      trend = trend, 
      var_phase = tmp_model$var_phase, 
      var_inter = tmp_model$var_inter
    )
  }

  if (!is.null(update)) formula <- update(formula, update)

  y <- as.matrix(data[, dvar])
 
  full <- lm(formula, data = data, na.action = na.action, ...)
  full$coef_std <- .std_lm(full)
  
  null <- lm(y ~ 1, na.action = na.action, ...)
  
  out <- structure(
    list(
      model = model, 
      contrast = list(level = contrast_level, slope = contrast_slope), 
      full.model = full,
      null_model = null,
      formula = formula
    ),
    class = c("sc_mplm")
  )

  attributes(out)[opts("phase", "mt", "dv")] <- list(pvar, mvar, dvar)
  out
}
