#' Read single-case data from files
#' 
#' Use the \code{readSC} function to import single-case data from structured
#' .csv or the \code{readSC.excel} function for importing excel files.
#' 
#' 
#' @aliases readSC readSC.excel
#' @param filename A character string defining the file to be imported (e.g.
#' \code{"SC_Anita.csv"}. If filename is left empty a dialog box for choosing
#' will be opened.
#' @param data A data frame. As an alternative to \code{filname} a dataframe could be 
#' directly provided. 
#' @param sep The field separator string. Values within rows have to be
#' separated by this string. Default is \code{sep = ","}.
#' @param dec The string used for decimal points in the file. Must be a single
#' character. Default is \code{dec = "."}
#' @param sort.labels If set TRUE, the resulting list is sorted by label names
#' (alphabetically increasing).
#' @param type Format of the file to be imported. Either "csv" or "excel" is
#' possible.
#' @param cvar Sets the variable name of the "case" variable. Deafults to \code{"case"}.
#' @param pvar Sets the variable name of the "phase" variable. Deafults to \code{"phase"}.
#' @param dvar Sets the variable name of the "values" variable. Deafults to \code{"values"}.
#' @param mvar Sets the variable name of the "mt" variable. Deafults to \code{"mt"}.
#' @param phase.names A character vector with phase names. Deafults to the phase names provided 
#' in the phase variable.
#' @param \dots Further arguments passed to the \code{\link{read.table}}
#' command.
#' @return Returns a single-case data frame. See \code{\link{scdf}} to learn
#' about the format of these data frames.
#' @author Juergen Wilbert
#' @seealso \code{\link{read.table}}, \code{\link{writeSC}}, \code{\link{scdf}}, \code{\link{readRDS}}
#' @keywords manip
#' @examples
#' 
#' ## Read SC-data from a file named "study1.csv" in your working directory
#' # study1 <- readSC("study1.csv")
#' 
#' ## Read SC-data from a .csv-file with semicolon as field and comma as decimal separator
#' # study2 <- readSC("study2.csv", sep = ";", dec = ",")
#' 
#' ## writeSc and readSC
#' filename <- file.path(tempdir(),"test.csv")
#' writeSC(exampleA1B1A2B2_zvt, filename)
#' dat <- readSC(filename, cvar = "case", pvar = "part", dvar = "zvt", mvar = "day")
#' res1 <- describeSC(exampleA1B1A2B2_zvt)$descriptives
#' res2 <- describeSC(dat)$descriptives
#' identical(res1,res2)
#' 
#' @export
readSC <- function(filename = NULL, data = NULL, sep = ",", dec = ".", sort.labels = FALSE, cvar = "case", pvar = "phase", dvar = "values", mvar = "mt", phase.names = NULL, type = "csv", ...) {
  if (is.null(filename) && is.null(data)) {
    filename <- file.choose()
    cat("Import file", filename, "\n\n")
  }
  if (!is.null(data)) {
    type <- "data"
    dat <- as.data.frame(data)
  }
  
  if (type == "csv")
    dat <- utils::read.table(filename, header = TRUE, sep = sep, dec = dec, stringsAsFactors = FALSE,...)
  if (type == "excel") {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Package readxl needed for this function to work. Please install it.",
           call. = FALSE)
    }
    #stop("Excel import currently not supported.")
    dat <- as.data.frame(readxl::read_excel(filename, ...))
  }

  VARS <- c(cvar, pvar, dvar, mvar)
  columns <- ncol(dat)
 
  #pos <- match(VARS, names(dat))
  #pos.rest <- which(!(1:columns %in% pos))
  #dat <- dat[, c(pos, pos.rest)]

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
  cat("Imported", length(dat), "cases.\n")
  #if (columns == 3) {
  #  cat("Measurement-times are missing. Standard times were assigned.\n")
  #  dat <- .SCprepareData(dat)
  #}
  class(dat) <- c("scdf","list")
  scdf_attr(dat, .opt$phase) <- pvar
  scdf_attr(dat, .opt$dv)    <- dvar
  scdf_attr(dat, .opt$mt)    <- mvar
  
  
  return(dat)
}

#' @rdname readSC
#' @export
readSC.excel <- function(...) {
  readSC(..., type = "excel")
  
}

