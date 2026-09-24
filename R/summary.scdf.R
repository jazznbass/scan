#' Summary function for an scdf object
#' 
#' Provides a summary of an `scdf` object, including the number of cases,
#' measurements per case, and design information.
#' 
#' @details The summary includes:
#' - Total number of cases in the `scdf`.
#' - A table listing each case with the number of measurements and design.
#' - Variable names with annotations for phase, measurement-time, and dependent variable.
#' - Additional information and author details if available.
#' 
#' `summary()` returns the summary object; the output is written by its print
#' method. So `summary(scdf)` shows the summary at the console as before, while
#' `export(summary(scdf))` and an assignment stay silent.
#' @author Juergen Wilbert
#' @param object An scdf object
#' @param all_cases If TRUE, more than 10 cases are summarized
#' @param ... not in use
#' @return An object of class `scdf_summary`.
#' @order 1
#' @export
summary.scdf <- function(object, all_cases = FALSE, ...) {
  
  attr(object, "all_cases") <- all_cases
  class(object) <- "scdf_summary"
  object
  
}
