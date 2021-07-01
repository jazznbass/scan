#' Draw random names
#'
#' @param n Number of names
#' @param type "neutral", "male", "female", or "mixed"
#' @param seed A seed for the random number generator.
#' @return A character vector with random names
#' @export
draw_names <- function(n = 1, type = "neutral", seed = NULL) {
  
  if (!is.null(seed)) set.seed(seed)
  out <- character(0)
  if (type == "neutral") out <- sample(.opt$neutral.names, n)
  if (type == "female") out <- sample(.opt$female.names, n)
  if (type == "male") out <- sample(.opt$male.names, n)
  if (type == "mixed") out <- sample(.opt$names, n)
  
  out
}

