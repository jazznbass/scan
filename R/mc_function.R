mc_function <- function(name) {
  
  if(missing(name)) name <- ""
  id <- which(names(.opt$mc_fun) == name)
  
  if (length(id) == 1) {
    return(.opt$mc_fun[id])
  }
  
  stop("Unknown function. Valid values are: ",
       paste(names(.opt$mc_fun), collapse = ", "))
  
}
