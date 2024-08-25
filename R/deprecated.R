#' List of old deprecated function names
#' 
#' This is a list with functions names that have been replaced by new names
#' which are in line with R syntax guidelines. The old function names still 
#' work. They are wrappers that call the new function.
#' 
#' @param ... Arguments passed through to the new function.
#' @name deprecated-functions
#' @keywords internal
NULL

#' @rdname deprecated-functions
#' @export
tauUSC <- function(...) {
  .deprecated_warning("tau_u", "tauUSC")
  tau_u(...)
}

#' @rdname deprecated-functions
#' @export
power_testSC <- function(...) {
  .deprecated_warning("power_test", "power_testSC")
  power_test(...)
}

#' @rdname deprecated-functions
#' @export
fillmissingSC <- function(...) {
  .deprecated_warning("fill_missing", "fillmissingSC")
  fill_missing(...)
}

#' @rdname deprecated-functions
#' @export
overlapSC <- function(...) {
  .deprecated_warning("overlap", "overlapSC")
  overlap(...)
}

#' @rdname deprecated-functions
#' @export
randSC <- function(...) {
  .deprecated_warning("rand_test", "randSC")
  rand_test(...)
}

#' @rdname deprecated-functions
#' @export
rand.test <- function(...) {
  .deprecated_warning("rand_test", "rand.test")
  rand_test(...)
}

#' @rdname deprecated-functions
#' @export
rciSC <- function(...) {
  rci(...)
}


#' @rdname deprecated-functions
#' @export
rSC <- function(...) {
  .deprecated_warning("random_scdf", "rSC")
  random_scdf(...)
}

