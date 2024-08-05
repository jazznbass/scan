

opt <- function(x) {
  out <- .opt[[x]]
  if (is.null(out)) stop("Option ", x, " not defined.")
  out
}


# Basic options -----------------------------------------------------------

.opt <- list(
  scdf         = "scdf",
  dv           = "var.values",
  phase        = "var.phase",
  mt           = "var.mt",
  case_name    = "name",
  info         = "info",
  author       = "author",
  rigorous_class_check = TRUE, 
  names_default = paste0("Case", 1:500),
  function_experimental_warning  = paste(
    "This function is in an experimental state.",
    "The syntax and behaviour will probably change in a future version."
  ),
  function_deprecated_warning = paste(
    "This function is deprecated. It might be dropped without any further",
    "notice in a future update of scan."
  ),
  style = list()
  )

.opt$citation <- {
  x <- citation("scan")
  class(x) <- "list"
  attributes(x[[1]])$textVersion
}

.opt$tip <- list(
    "Visit the free online book on scan at: https://jazznbass.github.io/scan-Book/",
    "For information on citing scan, type citation(\"scan\").",
    "Start the graphical user interface of scan with shinyscan().",
    "Post a question or improvement idea on scan at https://github.com/jazznbass/scan/discussions",
    "Post issues at https://github.com/jazznbass/scan/issues",
    "Find the online help-pages at https://jazznbass.github.io/scan/",
    "The addon package 'scplot' provides sophisticated graphs for single-case data.",
    "Set 'options(scan.export.engine = \"gt\")' and try out the improved table engine for 'export()'."
  )

.opt$startup_message <- paste0(
  "scan ",utils::packageVersion("scan")," (",utils::packageDate('scan'),")\n"
)

