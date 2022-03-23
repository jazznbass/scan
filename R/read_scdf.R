#' Load single-case data from files
#'
#' Use the \code{read_scdf} function to load single-case data csv, excel, or
#' yaml files.
#' @param filename A character string defining the file to be loaded (e.g.
#'   \code{"SC_Anita.csv"}. If left empty a dialog box for choosing will be
#'   opened.
#' @param data A data frame. As an alternative to \code{filname}.
#' @param sort.labels If set TRUE, the resulting list is sorted by label names
#'   (alphabetically increasing).
#' @param sep The field separator string. Values within each row of x are 
#' separated by this string.
#' @param dec The string to use for decimal points in numeric or complex 
#' columns: must be a single character.
#' @param type Format of the file to be loaded. Either "csv", "xlsx", "xls",
#'   "excel", "yml" is possible. By default (NA) the type is extracted from the
#'   file extension.
#' @param cvar Sets the variable name of the "case" variable. Defaults to
#'   \code{"case"}.
#' @param pvar Sets the variable name of the "phase" variable. Defaults to
#'   \code{"phase"}.
#' @param dvar Sets the variable name of the "values" variable. Defaults to
#'   \code{"values"}.
#' @param mvar Sets the variable name of the "mt" variable. Defaults to
#'   \code{"mt"}.
#' @param phase.names A character vector with phase names. Defaults to the phase
#'   names provided in the phase variable.
#' @param \dots Further arguments passed to the repective read function
#'   (\code{\link{read.table}}, \code{\link{read_excel}},
#'   \code{\link{read_yaml}} command.
#' @return Returns a single-case data frame. See \code{\link{scdf}} to learn
#'   about the format of these data frames.
#' @author Juergen Wilbert
#' @seealso \code{\link{read.table}}, \code{\link{writeSC}}, \code{\link{scdf}},
#'   \code{\link{readRDS}}
#' @keywords manip
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
read_scdf <- function(filename, 
                      data = NULL, 
                      sort.labels = FALSE, 
                      cvar = "case", 
                      pvar = "phase", 
                      dvar = "values", 
                      mvar = "mt", 
                      phase.names = NULL, 
                      sep = ",",
                      dec = ".",
                      type = NA, 
                      ...) {
  
  if (missing(filename) && is.null(data)) {
    filename <- file.choose()
    cat("Load file", filename, "\n\n")
  }
  
  if (is.na(type)) {
    type <- substring(filename, regexpr("\\.([[:alnum:]]+)$", filename) + 1)
  }
  
  if (!is.null(data)) {
    type <- "data"
    dat <- as.data.frame(data)
  }
  
  if (type == "csv") {
    dat <- utils::read.table(
      filename, 
      header = TRUE,
      stringsAsFactors = FALSE,
      sep = sep,
      dec = dec,
      ...
    )
  }
  
  if (type %in% c("yml", "yaml")) return(.load_yml(filename, ...))
  
  if (type %in% c("excel", "xlsx", "xls")) {
    dat <- as.data.frame(readxl::read_excel(filename, ...))
  }

  columns <- ncol(dat)

  if (!sort.labels) {
    dat[[cvar]] <- factor(dat[[cvar]], levels = unique(dat[[cvar]]))
  } else {
    dat[[cvar]] <- factor(dat[[cvar]])
  }
  
  dat[[pvar]] <- factor(dat[[pvar]], levels = unique(dat[[pvar]]))
  
  if (!is.null(phase.names)) levels(dat[[pvar]]) <- phase.names

  lab <- levels(dat[[cvar]])
  dat <- split(dat, dat[[cvar]])
  dat <- lapply(dat, function(x) x[, 2:columns])
  for(i in 1:length(dat)) row.names(dat[[i]]) <- 1:nrow(dat[[i]])
  names(dat) <- lab
  cat("Loaded", length(dat), "cases.\n")

  class(dat) <- c("scdf", "list")
  scdf_attr(dat, .opt$phase) <- pvar
  scdf_attr(dat, .opt$dv)    <- dvar
  scdf_attr(dat, .opt$mt)    <- mvar
  
  
  dat
}

#' @rdname read_scdf
#' @export
readSC.excel <- function(...) {
  
  read_scdf(..., type = "excel")
  
}

#' @rdname read_scdf
#' @export
readSC <- function(...) {
  read_scdf(...)
}


#' @export
.load_yml <- function(filename, ...) {
  
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
