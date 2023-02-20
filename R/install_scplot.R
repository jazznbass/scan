#' Installation instructions for scplot
#' 
#' scplot is a new extended plotting interface for scan based on ggplot2.
#' It is not yet implemented into scan or available on cran and must be 
#' installed from github. This functions gives instructions how to do this.
#' 
#' @export
install_scplot <- function() {
  devtools <- .is_installed("devtools")
  scplot <- .is_installed("scplot")
  
  if (!scplot) {
    cat("The 'scplot' package is not installed.\n")
    
    if (!devtools) {
      cat("\nTo install scplot, you first need to install devtools with:\n")
      cat('install.packages("devtools")')
    }
    cat("\n")
    cat("\nNow you can install scplot with:\n")
    cat('devtools::install_github("jazznbass/scplot")\n')
    cat("\nSorry, but that's the way :-)\n")
    cat("\n")
    if (Sys.info()['sysname'] == "Windows") {
      cat("As a Windows users, you will also need to install Rtools.\n")
      cat("You can download and install it from:\n")
      cat("https://cran.r-project.org/bin/windows/Rtools/\n")
      cat("Note: I cannot check if you already have rtools installed.\n")
      cat("So this information may be superfluous.\n")
    }
    
  } else cat("Type library(scplot) to start scplot")
}
