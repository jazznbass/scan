#' Trend analysis
#'
#' Intercept, regression weight and standardized regression weight of a linear
#' and a quadratic regression of the values on the measurement time, computed
#' separately for each phase and across all phases of one case.
#'
#' @details `trend()` takes a single case; an scdf with more than one case
#'   raises an error. Select a case with `[` or `$` beforehand.
#'
#'   Two models are computed by default, a linear and a quadratic one. Further
#'   models are added with `model`, written as R formulas in the variables
#'   `values` and `mt`. Every model must have exactly one predictor; a model
#'   with more raises an error.
#'
#'   Each model is fitted once to the whole series and once within each phase.
#'   Before fitting, the measurement time is shifted so that the earliest
#'   measurement of that phase — of the whole series for the model across all
#'   phases — becomes `first_mt`. The intercept is therefore the estimated
#'   value at the beginning of the phase, which is what makes the intercepts of
#'   the phases comparable. `first_mt = 1` is needed for models that are
#'   undefined at zero, such as `values ~ log(mt)`.
#'
#'   The rows of the result are named `<model>.<phase>`, with `ALL` for the
#'   model across all phases. A phase name that occurs more than once is
#'   numbered, so an ABAB design gives the phases `A(1)`, `B(1)`, `A(2)` and
#'   `B(2)`, matching the phase names [describe()] uses.
#'
#'   Measurements with a missing value in the dependent variable or the
#'   measurement time are dropped from the model they belong to.
#' @inheritParams .inheritParams
#' @param first_mt A numeric setting the value the first measurement time of
#'   each phase is shifted to.
#' @param offset Deprecated, use `first_mt`. A numeric value sets `first_mt` to
#'   `offset + 1`.
#' @param model A string or a list of (named) strings each depicting one
#'   regression model. This is a formula expression of the standard R class. The
#'   parameters of the model are `values` and `mt`.
#' @return An object of class `sc_trend` with the elements:
#'  |  |  |
#'  | --- | --- |
#'  | `trend` | Data frame with the columns `Intercept`, `B` and `Beta` and one row per model and phase. |
#'  | `formulas` | The formulas of all computed models. |
#'  | `phase_names` | The phase names as they appear in the row names. |
#' @author Juergen Wilbert
#' @family regression functions
#' @seealso [describe()]
#' @examples
#' trend(exampleAB[1])
#'
#' # a cubic model and the values predicted by the log of the measurement time
#' trend(
#'   exampleAB[1],
#'   model = list("Cubic" = values ~ mt^3, "Log Time" = values ~ log(mt)),
#'   first_mt = 1
#' )
#' @order 1
#' @export
trend <- function(data, dvar, pvar, mvar, 
                  offset = "deprecated",
                  first_mt = 0,
                  model = NULL) {

  check_args(
    has_length(data, 1, "trend can not be applied to more than one case.")
  )
  
  if (is.numeric(offset)) first_mt <- offset + 1
  
  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- dv(data) else dv(data) <- dvar
  if (missing(pvar)) pvar <- phase(data) else phase(data) <- pvar
  if (missing(mvar)) mvar <- mt(data) else  mt(data) <- mvar
  
  data <- .prepare_scdf(data)
  data <- data[[1]]
  
  phase_names <- rle(
    as.character(rename_phase_duplicates(data[[pvar]]))
  )$values
  
  phases <- .phasestructure(data, pvar = pvar)
  
  formulas <- c(
    "Linear" = formula(paste0(dvar, " ~ ", mvar)) , 
    "Quadratic" = formula(paste0(dvar, " ~ I(", mvar, "^2)"))
  )
  formulas_names <- c("Linear", "Quadratic")
  if(!is.null(model)) {
    formulas <- c(formulas, model)
    formulas_names <- names(formulas)
  }
  tmp <- length(phase_names) + 1
  rows <- paste0(paste0(rep(formulas_names, each = tmp), "."), c("ALL", phase_names))
  
  ma <- matrix(NA, nrow = length(rows), ncol = 3)
  row.names(ma) <- rows
  colnames(ma) <- c("Intercept", "B", "Beta")
  ma <- as.data.frame(ma)
  
  for(i_formula in 1:length(formulas)) {
    data_phase <- data
    mvar_correction <- min(data_phase[[mvar]], na.rm = TRUE) - first_mt
    data_phase[[mvar]] <- data_phase[[mvar]] - mvar_correction
    
    .row <- which(rows == paste0(formulas_names[i_formula], ".ALL"))
    coefs <- .beta_weights(
      lm(formulas[[i_formula]], data = data_phase, na.action = na.omit)
    )
    if (length(coefs) != 3L) {
      abort(
        "Model '", formulas_names[i_formula], "' has ",
        (length(coefs) - 1L) %/% 2L, " predictors. trend() only ",
        "supports models with exactly one predictor."
      )
    }
    ma[.row, 1:3] <- coefs
    
    for(p in 1:length(phase_names)) {
      data_phase <- data[phases$start[p]:phases$stop[p], ]
      mvar_correction <- min(data_phase[[mvar]], na.rm = TRUE) - first_mt
      data_phase[[mvar]] <- data_phase[[mvar]] - mvar_correction 
      .row <- which(rows == paste0(formulas_names[i_formula], ".", phase_names[p]))
      ma[.row, 1:3] <- .beta_weights(
        lm(formulas[[i_formula]], data = data_phase, na.action = na.omit)
      )
    }
  }
  
  out <- list(
    trend = ma, 
    first_mt = first_mt,
    formulas = formulas, 
    offset = offset, 
    phase_names = phase_names
  )
  class(out) <- c("sc_trend")
  attributes(out)[opts("phase", "mt", "dv")] <- list(pvar, mvar, dvar)
  out
}
