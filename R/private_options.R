

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

.opt$startup_message <- paste0(
  "\033[34m", 
  "scan ",utils::packageVersion("scan")," (",utils::packageDate('scan'),")\n",
  #"Single-Case Data Analysis for Single and Multiple Baseline Designs\n",
  "\033[31m",
  paste0("For information on citing scan, type citation(\"scan\").")
)

