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

  on.exit(print_messages(pretext = "Note:"))
  
  original_attr <- scdf_attr(object)
  if (!is.null(original_attr)) {
    pvar <- phase(object)
    dvar <- dv(object)
    mvar <- mt(object)
    add_message("Found scdf attributes and replaced function arguments.")
  }
  
  ## check file -----
  
  check_args(
    is_true(dvar %in% names(object), "Variable '", dvar, "' is missing."),
    is_true(mvar %in% names(object), "Variable '", mvar, "' is missing."),
    is_true(pvar %in% names(object), "Variable '", pvar, "' is missing."),
    not(any(is.na(object[[cvar]])), "Variable '", cvar, "' has a missing value."),
    not(any(is.na(object[[pvar]])), "Variable '", pvar, "' has a missing value.")
  )
  
  if (!cvar %in% names(object)) {
    add_message("Casename variable not found. Assuming one case.")
    object[[cvar]] <- "unnamed"
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
  
  if (!is.null(original_attr)) {
    scdf_attr(object) <- original_attr
  } else {
    phase(object) <- pvar
    dv(object) <- dvar
    mt(object) <- mvar
  }

  object

}
