#' Multivariate Piecewise linear model / piecewise regression
#'
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
#' res <- mplm(Leidig2018$`1a1`, dvar = c("academic_engagement", "disruptive_behavior"))
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
                 na.action = na.omit, ...) {
 
  model <- match.arg(model)
  contrast <- match.arg(contrast)
  contrast_level <- match.arg(contrast_level)
  contrast_slope <- match.arg(contrast_slope)
  
  if (is.na(contrast_level)) contrast_level <- contrast
  if (is.na(contrast_slope)) contrast_slope <- contrast
  
  if (model == "JW") {
    contrast_level <- "preceding"
    contrast_slope <- "preceding"
    model <- "B&L-B"
  }
  
  check_args(
    one_of(model, c("H-M", "B&L-B", "W")),
    one_of(contrast, c("first", "preceding"))
  )
  #start_check() %>%
  #  check_in(model, c("H-M", "B&L-B", "W")) %>%
  #  check_in(contrast, c("first", "preceding")) %>%
  #  end_check()
  
  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- scdf_attr(data, .opt$dv) else scdf_attr(data, .opt$dv) <- dvar
  if (missing(pvar)) pvar <- scdf_attr(data, .opt$phase) else scdf_attr(data, .opt$phase) <- pvar
  if (missing(mvar)) mvar <- scdf_attr(data, .opt$mt) else scdf_attr(data, .opt$mt) <- mvar

  data <- .prepare_scdf(data)

  N <- length(data)
  if (N > 1) {
    stop("Procedure could not be applied to more than one case.\n")
  }

  ### model definition
  tmp_model <- .add_model_dummies(
    data = data, model = model, 
    contrast_level = contrast_level, contrast_slope = contrast_slope
  )
  
  data <- tmp_model$data[[1]]

  if (is.null(formula)) {
    formula <- .create_fixed_formula(
      dvar = "y", mvar = mvar, slope = slope, level = level,
      trend = trend, var_phase = tmp_model$var_phase, var_inter = tmp_model$var_inter
    )
    formula <- as.formula(formula)
  }

  if (!is.null(update)) formula <- update(formula, update)

  y <- as.matrix(data[, dvar])

  full <- lm(formula, data = data, na.action = na.action, ...)
  full$coef_std <- .std_lm(full)
  out <- list(model = model, 
              contrast = list(level = contrast_level, slope = contrast_slope), 
              full.model = full, formula = formula)

  class(out) <- c("sc_mplm")
  attr(out, .opt$phase) <- pvar
  attr(out, .opt$mt) <- mvar
  attr(out, .opt$dv) <- dvar
  out
}
