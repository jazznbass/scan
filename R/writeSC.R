#' Export data into a .csv-file
#' 
#' This function restructures and exports single-case data into a .csv-file.
#' 
#' @inheritParams .inheritParams
#' @param filename A character string defining the output file name (e.g.
#' \code{"SC_data.csv"}.
#' @param sep The field separator string. Values within rows will be separated
#' by this string. Default is \code{sep = ","}.
#' @param dec The string used for decimal points. Must be a single character.
#' Default is \code{dec = "."}
#' @param \dots Further arguments passed to write.table.
#' @author Juergen Wilbert
#' @seealso \code{\link{write.table}}, \code{\link{readSC}},
#' \code{\link{saveRDS}}
#' @keywords manip
#' @examples
#' ## Write single-case data to a .csv-file
#' filename <- tempfile()
#' jessica <- rSC(design_rSC(level = .5))
#' writeSC(jessica, filename)
#' 
#' ## Write multiple cases to a .csv-file with semicolon as field and comma as decimal separator
#' writeSC(Grosche2011, filename, sep = ";", dec = ",")
#' 
#' ## writeSC and readSC
#' writeSC(exampleA1B1A2B2_zvt, filename)
#' dat <- readSC(filename, cvar = "case", pvar = "part", dvar = "zvt", mvar = "day")
#' res1 <- describe(exampleA1B1A2B2_zvt)$descriptives
#' res2 <- describe(dat)$descriptives
#' all.equal(res1,res2)
#' @export
writeSC <- function(data, filename = NULL, sep = ",", dec = ".", ...) {
  if(is.null(filename)) {
    filename <- file.choose()
    cat("Write to file", filename, "\n\n")
  }
  
  utils::write.table(
    as.data.frame(data), 
    file = filename, 
    sep = sep, 
    dec = dec, 
    row.names = FALSE, 
    ...
  )
  
}
