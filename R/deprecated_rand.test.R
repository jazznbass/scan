#' (deprecated) Randomization Test
#' 
#' Please use the randSC function
#'
#' @aliases rand.test
#' @param ... Passed to randSC
#' @export
rand.test <- function(...) {
  warning(.opt$function_deprecated_warning, "Please use: randSC instead.")
  randSC(...)
}
