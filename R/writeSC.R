#' Saves data into a .csv-file
#' 
#' This function restructures and saves single-case data into a .csv-file.
#' 
#' @inheritParams .inheritParams
#' @param filename A character string defining the output file name (e.g.
#' \code{"SC_data.csv"}.
#' @param \dots Further arguments passed to write.table.
#' @author Juergen Wilbert
#' @seealso \code{\link{write.table}}, \code{\link{readSC}},
#' \code{\link{saveRDS}}
#' @keywords manip
#' @examples
#' ## Save single-case data to a .csv-file
#' filename <- filename <- file.path(tempdir(), "test.csv")
#' jessica <- random_scdf(design(level = .5))
#' save_scdf(jessica, filename)
#' 
#' ## Save multiple cases to a .csv-file with semicolon as field and comma as decimal separator
#' save_scdf(Grosche2011, filename, sep = ";", dec = ",")
#' 
#' ## save_scdf and load_scdf
#' save_scdf(exampleA1B1A2B2_zvt, filename)
#' dat <- load_scdf(filename, cvar = "case", pvar = "part", dvar = "zvt", mvar = "day")
#' res1 <- describe(exampleA1B1A2B2_zvt)$descriptives
#' res2 <- describe(dat)$descriptives
#' all.equal(res1,res2)
#' @export
save_scdf <- function(data, filename = NULL, ...) {
  if(is.null(filename)) {
    filename <- file.choose()
    cat("Save to file", filename, "\n\n")
  }
  
  utils::write.table(
    as.data.frame(data), 
    file = filename, 
    row.names = FALSE, 
    ...
  )
  
}

#' @rdname save_scdf
#' @export
writeSC <- function(...) {
  save_scdf(...)
}
