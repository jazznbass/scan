#' Print methods for scan objects
#'
#' @param x Object 
#' @param digits The minimum number of significant digits to be use. 
#' If set to "auto" (default), values are predefined.
#' @param ... Further parameters passed to the print function
#' @name print.sc
#' @keywords internal
NULL


.note_vars <- function(x) {

  var_dv    <- attr(x, opt("dv"))
  var_phase <- attr(x, opt("phase"))
  var_mt    <- attr(x, opt("mt"))

  # attributes that are not set are neither checked nor reported
  changed <- c(
    any(var_dv    != "values"),
    any(var_phase != "phase"),
    any(var_mt    != "mt")
  )
  if (!any(changed)) return(invisible(NULL))

  parts <- c(
    if (!is.null(var_dv)) paste0(
      "'", paste0(var_dv, collapse = "/ "), "' as dependent variable"
    ),
    if (!is.null(var_phase)) paste0(
      "'", paste0(var_phase, collapse = "/ "), "' as phase variable"
    ),
    if (!is.null(var_mt)) paste0(
      "'", paste0(var_mt, collapse = "/ "), "' as measurement-time variable"
    )
  )
  if (length(parts) > 1) {
    parts[length(parts)] <- paste0("and ", parts[length(parts)])
  }

  cat(
    "\nThe following variables were used in this analysis:\n",
    paste0(parts, collapse = ", "), ".\n",
    sep = ""
  )
}

