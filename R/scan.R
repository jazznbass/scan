.onAttach <- function(lib, pkg, ...) {
	#out <- paste0("scan ", utils::packageVersion("scan"), 
	#              " - Experimental (", utils::packageDate('scan'), ")\n",
	#              "Single-Case Data Analysis for Single and Multiple Baseline Designs\n")
  packageStartupMessage(.opt$startup_message)
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
    scan.print.scdf.name = TRUE,
    scan.plot.style = "grid",
    scan.scplot.theme = "grid",
    scan.deprecated.warning = FALSE,
    scan.export.kable = list(digits = 2, linesep ="", booktab = TRUE),
    scan.export.kable_styling = list(
      bootstrap_options = c("bordered", "condensed"), 
      full_width = FALSE, position = "left",
      latex_options = c("hold_position"),
      htmltable_class = "lightable-classic"
    )
  )
  
  toset <- !(names(op_scan) %in% names(op))
  if (any(toset)) options(op_scan[toset])
  
  invisible()
  
}



.onAttach()



