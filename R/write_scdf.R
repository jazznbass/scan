#' Data output: Write single-case data to a .csv-file
#'
#' This function restructures and writes single-case data into a .csv-file.
#'
#' This is a wrapper for the write.table function with predefined parameters.
#' It converts the single-case data file into a data frame and writes it to a
#' .csv-file. Each single-case data set is stacked below each other and an
#' additional column "case" is added to identify the different single-cases.
#'
#' @inheritParams .inheritParams
#' @param filename A character string defining the output file name (e.g.
#'   `"scdf_data.csv"`. If `NULL` (default), the data are written to the
#'   console.
#' @param sep The field separator string. Values within each row of x are
#'   separated by this string.
#' @param dec The string to use for decimal points in numeric or complex
#'   columns: must be a single character.
#' @param \dots Further arguments passed to write.table.
#' @author Juergen Wilbert
#' @return Invisibly returns `NULL`.
#' @seealso [write.table()], [saveRDS()]
#' @family io-functions
#' @keywords io
#' @examples
#' ## write single-case data to a .csv-file
#' filename <- tempfile(fileext = ".csv")
#' write_scdf(exampleAB, filename)
#'
#' ## write multiple cases to a .csv-file with semicolon as field and comma as
#' ## decimal separator
#' write_scdf(Grosche2011, filename, sep = ";", dec = ",")
#'
#' ## read_scdf and write_scdf
#' write_scdf(exampleA1B1A2B2_zvt, filename)
#' dat <- read_scdf(filename, cvar = "case", pvar = "part",
#'                  dvar = "zvt", mvar = "day")
#' res1 <- describe(exampleA1B1A2B2_zvt)$descriptives
#' res2 <- describe(dat)$descriptives
#' all.equal(res1,res2)
#' @export
write_scdf <- function(data, filename = NULL, sep = ",", dec = ".", ...) {
  
  utils::write.table(
    as.data.frame(data), 
    file = filename, 
    row.names = FALSE,
    sep = sep,
    dec = dec,
    ...
  )
  
}

#' @rdname deprecated-functions
#' @export
writeSC <- function(...) {
  write_scdf(...)
}
