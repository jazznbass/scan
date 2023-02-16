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
  neutral.names = .neutral.names,
  female.names = .female.names,
  male.names   = .male.names,
  names        = .names,
  function_experimental_warning  = "This function is in an experimental state. The syntax and behaviour will probably change in a future version.",
  function_deprecated_warning = "This function is deprecated. It might be dropped without any further notice in a future update of scan.",
  #startup_message = paste0(
  #  "\033[34m", 
  #  "scan ",utils::packageVersion("scan")," (",utils::packageDate('scan'),")\n",
  #  "Single-Case Data Analysis for Single and Multiple Baseline Designs\n",
  #  "\033[31m",
  #  "\n",
  #  .opt$citation
  #), 
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

