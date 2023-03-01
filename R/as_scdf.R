#' as_scdf
#'
#' Converts a data frame to an scdf object.
#'
#' @param object A data.frame
#' @param cvar Sets the "case" variable. Defaults to `case`.
#' @param pvar Sets the "phase" variable. Defaults to `phase`.
#' @param dvar Sets the "values" variable. Defaults to `values`.
#' @param mvar Sets the variable name of the "mt" variable. Defaults to `mt`.
#' @param phase_names A character vector with phase names. Defaults to the phase
#'   names provided in the phase variable.
#' @param sort_cases If set TRUE, the resulting list is sorted by label names
#'   (alphabetically increasing).
#' @return An scdf.
#' @family data manipulation functions
#' @export

as_scdf <- function(object,                       
                    cvar = "case", 
                    pvar = "phase", 
                    dvar = "values", 
                    mvar = "mt", 
                    phase_names = NULL,
                    sort_cases = FALSE) {
  
  
  if (!cvar %in% names(object)) {
    message("Casename variable not found. Assuming one case.")
    object[[cvar]] <- "unnamed"
  }
  
  if (!is.null(attr(object, opt("scdf")))) {
    pvar <- attr(object, opt("scdf"))[[opt("phase")]] 
    dvar <- attr(object, opt("scdf"))[[opt("dv")]] 
    mvar <- attr(object, opt("scdf"))[[opt("mt")]] 
    message("Found scdf attributes and replaced function arguments.")
  }
  
  if (!sort_cases) {
    object[[cvar]] <- factor(object[[cvar]], levels = unique(object[[cvar]]))
  } else {
    object[[cvar]] <- factor(object[[cvar]])
  }
  
  object[[pvar]] <- factor(object[[pvar]], levels = unique(object[[pvar]]))
  if (!is.null(phase_names)) levels(object[[pvar]]) <- phase_names
  
  case_names <- levels(object[[cvar]])
  object <- split(object, object[[cvar]])
  object <- lapply(object, function(x) {
    x[, -which(names(x) == cvar)]
  })
  names(object) <- case_names
  
  class(object) <- c("scdf", "list")
  scdf_attr(object, opt("phase")) <- pvar
  scdf_attr(object, opt("dv"))    <- dvar
  scdf_attr(object, opt("mt"))    <- mvar
  
  object
  
}
