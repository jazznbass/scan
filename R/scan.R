.onAttach <- function(lib, pkg, ...) {
  packageStartupMessage(
    "\033[34m", 
    opt("startup_message"),
    "\033[31m",
    opt("tip")[[sample(1:length(opt("tip")), 1)]]
  )
  
}	

.onLoad <- function(lib, pkg, ...) {
  # global options ----------------------------------------------------------
  op <- options()
  op_scan <- list(
    scan.print.cases = "fit",
    scan.print.rows   = 15,
    scan.print.cols   = "all",
    scan.print.digits = 2,
    scan.print.long   = FALSE,
    scan.print.bar   = "|",
    scan.print.scdf.name = TRUE,
    scan.plot.style = "grid",
    scan.deprecated.warning = TRUE,
    scan.export.engine = "kable",
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



