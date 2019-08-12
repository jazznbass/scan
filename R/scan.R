.onAttach <- function(lib, pkg, ...) {
	out <- paste0("scan ", utils::packageVersion("scan"), 
	              " (", utils::packageDate('scan'), ")\n",
	              "Single-Case Data Analysis for Single and Multiple Baseline Designs\n")
	packageStartupMessage(out)
}	

.onLoad <- function(lib, pkg, ...) {
  # global options ----------------------------------------------------------
  op <- options()
  op_scan <- list(
    scan.print.cases = "fit",
    scan.print.rows   = 15,
    scan.print.cols   = "all",
    scan.print.digits = 1,
    scan.print.long   = FALSE,
    scan.print.scdf.name = TRUE
  )
  
  toset <- !(names(op_scan) %in% names(op))
  if (any(toset)) options(op_scan[toset])
  
  invisible()
  
}



.onAttach()



