#' List of old deprecated function names
#' 
#' This is a list with functions names that have been replaced by new names
#' which are in line with R syntax guidelines. The old function names still 
#' work. They are wrappers that call the new function.
#' 
#' @param ... Arguments passed through to the new function.
#' @name deprecated-functions
NULL

#' @rdname deprecated-functions
#' @export
design_rSC <- function(...) {
  .deprecated_warning("design", "design_rSC")
  design(...)
}

#' @rdname deprecated-functions
#' @export
longSCDF <- function(...) {
  as.data.frame(...)
}

#' @rdname deprecated-functions
#' @export
corrected_tauSC <- function(...) {
  .deprecated_warning("corrected_tau", "corrected_tauSC")
  corrected_tau(...)
}

#' @rdname deprecated-functions
#' @export
trendSC <- function(...) {
  .deprecated_warning("trend", "trendSC")
  trend(...)
}

#' @rdname deprecated-functions
#' @export
truncateSC <- function(...) {
  .deprecated_warning("truncate_phase", "truncateSC")
  truncate_phase(...)
}

#' @rdname deprecated-functions
#' @export
style_plotSC <- function(...) {
  .deprecated_warning("style_plot", "style_plotSC")
  style_plot(...)
}

#' @rdname deprecated-functions
#' @export
style.plotSC <- function(...) {
  .deprecated_warning("style_plot", "style.plotSC")  
  style_plot(...)
}

#' @rdname deprecated-functions
#' @export
smoothSC <- function(...) {
  smooth_cases(...)
}

#' @rdname deprecated-functions
#' @export
shiftSC <- function(...) shift(...)

#' @rdname deprecated-functions
#' @export
tauUSC <- function(...) {
  .deprecated_warning("tau_u", "tauUSC")
  tau_u(...)
}

#' @rdname deprecated-functions
#' @export
describeSC <- function(...) {
  .deprecated_warning("describe", "describeSC")
  describe(...)
}
