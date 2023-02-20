#' Installation instructions for shiny
#' 
#' shinyscan is a graphical user interface for scan based on shiny.
#' It is not yet implemented into scan or available on cran and must be 
#' installed from github along with the scplot package.
#' 
#' @export
install_shinyscan <- function() {
  shiny <- .is_installed("shinyscan")
  devtools <- .is_installed("devtools")
  scplot <- .is_installed("scplot")
  
  if (!shiny) {
    cat("The 'shinyscan' package is not installed.\n")
    
    if (!devtools || !scplot) {
      cat("\nTo install shinyscan, you first need to install:\n")
    }
    if (!devtools) cat('- devtools with install.packages("devtools")\n')
    if (!scplot) cat('- scplot with',
                     'devtools::install_github("jazznbass/scplot")\n')
    cat("\n")
    cat("\nNow you can install shinyscan with:\n")
    cat('devtools::install_github("jazznbass/shinyscan")\n')
    cat("\nSorry, but that's the way :-)\n")
    cat("\n")
    if (Sys.info()['sysname'] == "Windows") {
      cat("As a Windows users, you will also need to install Rtools.\n")
      cat("You can download and install it from:\n")
      cat("https://cran.r-project.org/bin/windows/Rtools/\n")
      cat("Note: I cannot check if you already have rtools installed.\n")
      cat("So this information may be superfluous.\n")
    }
    
  } else cat("Start shinyscan with: shinyscan::shinyscan()")
}

.is_installed <- function(x) {
  res <- try(find.package(x), silent = TRUE)
  if (inherits(res, "character")) return(TRUE) else return(FALSE)
}

