#' Returns a function to use in Power analysis and Monte-Carlo studies
#'
#' @param name Character string. Identifier of function.
#' 
#' @details If 'name' is unknown, a list with valid names is provided.
#' @return A function.
#' @export

mc_function <- function(name) {
  id <- which(names(.opt$mc_fun) == name)
  
  if (length(id) == 1) {
    return(.opt$mc_fun[id])
  }
  
  stop("Unknown function. Valid values are: ",
       paste(names(.opt$mc_fun), collapse = ", "))
  
}