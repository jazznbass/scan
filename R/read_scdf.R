#' Load single-case data from files
#'
#' Use the `read_scdf` function to load single-case data csv, excel, or yaml
#' files.
#'
#' @param file Either a character string defining the file to be loaded (e.g.
#'   `"SC_Anita.csv"` (if left empty a dialog box for choosing will be opened)
#'   or a data.frame.
#' @param cvar Sets the variable name of the "case" variable. Defaults to
#'   `"case"`.
#' @param pvar Sets the variable name of the "phase" variable. Defaults to `"phase"`.
#' @param dvar Sets the variable name of the "values" variable. Defaults to `"values"`.
#' @param mvar Sets the variable name of the "mt" variable. Defaults to `"mt"`.
#' @param sort_cases,sort.labels If set TRUE, the resulting list is sorted by
#'   label names (alphabetically increasing).
#' @param phase_names,phase.names A character vector with phase names. Defaults
#'   to the phase names provided in the phase variable.
#' @param type Format of the file to be loaded. Either "csv", "xlsx", "xls",
#'   "excel", "yml" is possible. By default (NA) the type is extracted from the
#'   file extension.
#' @param na Character vector of strings to interpret as missing values.
#' @param \dots Further arguments passed to the respective read function.
#' @return Returns a single-case data frame. See \code{\link{scdf}} to learn
#'   about the format of these data frames.
#' @author Juergen Wilbert
#' @seealso [read.table()], [readRDS()]
#' @family io-functions
#' @keywords io
#' @examples
#'
#' ## Read SC-data from a file named "study1.csv" in your working directory
#' # study1 <- read_scdf("study1.csv")
#'
#' ## Read SC-data from a .csv-file with semicolon as field and comma as decimal separator
#' # study2 <- read_scdf("study2.csv", sep = ";", dec = ",")
#'
#' ## write_scdf and read_scdf
#' filename <- file.path(tempdir(), "test.csv")
#' write_scdf(exampleA1B1A2B2_zvt, filename)
#' dat <- read_scdf(filename, cvar = "case", pvar = "part", dvar = "zvt", mvar = "day")
#' res1 <- describe(exampleA1B1A2B2_zvt)$descriptives
#' res2 <- describe(dat)$descriptives
#' all.equal(res1,res2)
#'
#' @export
read_scdf <- function(file, 
                      cvar = "case", 
                      pvar = "phase", 
                      dvar = "values", 
                      mvar = "mt",
                      sort_cases = FALSE,
                      phase_names = NULL, 
                      type = NA, 
                      na = c("", "NA"),
                      sort.labels = NULL, 
                      phase.names = NULL, 
                      ...) {
  
  if (missing(file)) {
    file <- file.choose()
    cat("Load file", file, "\n\n")
  }
  
  if(!is.null(phase.names)) phase_names <- phase.names
  if(!is.null(sort.labels)) sort_cases <- sort.labels
  
  if (!inherits(file, "character")) {
    type <- "data"
  }
  
  if (is.na(type)) {
    type <- substring(file, regexpr("\\.([[:alnum:]]+)$", file) + 1)
  }
  
  if (type == "csv") {
    dat <- utils::read.csv(
      file, 
      header = TRUE,
      stringsAsFactors = FALSE,
      na.strings = na,
      ...
    )
  }
  
  if (type %in% c("yml", "yaml")) return(.load_yml(file, ...))
  
  if (type %in% c("excel", "xlsx", "xls")) {
    dat <- as.data.frame(readxl::read_excel(file, na = na,...))
  }

  if (type == "data") {
    dat <- as.data.frame(file)
  }
  
  out <- as_scdf(
    dat, 
    sort_cases = sort_cases, 
    cvar = cvar, 
    pvar = pvar, 
    dvar = dvar, 
    mvar = mvar, 
    phase_names = phase_names   
  )
  
  message("Imported ", length(out), " cases")
  out
}

#' @rdname deprecated-functions
#' @export
readSC.excel <- function(...) {
  
  read_scdf(..., type = "excel")
  
}

#' @rdname deprecated-functions
#' @export
readSC <- function(...) {
  read_scdf(...)
}

.load_yml <- function(filename, ...) {
  
  if (!requireNamespace("yaml", quietly = TRUE)) {
    stop("You need to install the 'yaml' package first (install.packages('yaml')")
  }
  
  out <- yaml::read_yaml(filename, ...)
  
  case_names <- names(out)
  
  extract <- function(x) {
    
    if (is.null(x[["dvar"]])) x[["dvar"]] <- "values"
    if (is.null(x[["mvar"]])) x[["mvar"]] <- "mt"
    if (is.null(x[["pvar"]])) x[["pvar"]] <- "phase"
    
    dvar <- x[["dvar"]]
    pvar <- x[["pvar"]]
    mvar <- x[["mvar"]]
    
    x[["dvar"]] <- NULL
    x[["mvar"]] <- NULL
    x[["pvar"]] <- NULL
    
    phase <- numeric(0)
    values <- numeric(0)
    
    for(i in seq_along(x[[dvar]])) {
      phase <- c(
        phase, rep(names(x[[dvar]][i]), length(x[[dvar]][[i]])))
      values <- c(values, x[[dvar]][[i]])
    }
    
    x[[pvar]] <- phase
    x[[dvar]] <- values
    
    if (is.null(x[[mvar]])) x[[mvar]] <- 1:length(x[[dvar]])
    
    x <- list(as.data.frame(x))
    scdf_attr(x, "var.phase") <- pvar
    scdf_attr(x, "var.values") <- dvar
    scdf_attr(x, "var.mt") <- mvar
    class(x) <- c("scdf", "list")
    
    x
    
  }
  
  out <- lapply(out, extract)
  
  for(i in 1:length(out)) names(out[[i]]) <- case_names[i]
  
  for(i in 2:length(out)) scdf <- c(out[[i-1]], out[[i]])
  
  
  scdf
}
