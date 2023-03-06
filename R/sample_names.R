#' Samples random names
#'
#' @param n Number of names
#' @param type "neutral", "male", "female", or "mixed"
#' @param seed A seed for the random number generator.
#' @return A character vector with random names
#' @examples 
#' sample_names(3)
#' @export
sample_names <- function(n = 1, type = "neutral", seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  out <- character(0)
  if (type == "neutral") out <- sample(case_names$neutral, n)
  if (type == "female") out <- sample(case_names$female, n)
  if (type == "male") out <- sample(case_names$male, n)
  if (type == "mixed") out <- sample(case_names$all, n)
  
  out
}

