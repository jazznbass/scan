#' Creating a long format data frame from several single-case data frames
#' (scdf).
#' 
#' The longSCDF function transposes a scdf into one long data frame.
#' Additionally, a data frame can be merged that includes data of the subjects.
#' This might be helpful to prepare data to be used with other packages than scan.
#' 
#' 
#' @inheritParams .inheritParams
#' @param l2 A data frame providing additional variables at Level 2. The scdf
#' has to have names for all cases and the Level 2 data frame has to have a
#' column with corresponding case names.
#' @param id Variable name of the Level 2 data frame that contains the case
#' names.
#' @return Returns one data frame with data of all single-cases structured by
#' the case variable.
#' @author Juergen Wilbert
#' @family data manipulation functions
#' @keywords manip
#' @examples
#' 
#' ## Convert the list of three single-case data frames from Grosche (2011) into one long data frame
#' Grosche2011
#' Grosche2011_long <- longSCDF(Grosche2011)
#' Grosche2011_long
#' 
#' ## Combine an scdf with data for l2
#' Leidig2018_long <- longSCDF(Leidig2018, l2 = Leidig2018_l2)
#' names(Leidig2018_long)
#' summary(Leidig2018_long)
#' 
#' @export
longSCDF <- function(data, l2 = NULL, id = "case") {

  label <- .case.names(names(data), length(data))

  outdat <- vector()
  
  for (case in 1:length(data)) {
    data[[case]]$case <- label[case]
    outdat <- rbind(outdat, data[[case]])
  }
  
  outdat <- cbind(outdat[, ncol(outdat)], outdat[, -ncol(outdat)])
  colnames(outdat)[1] <- id
  
  if(!is.null(l2)) outdat <- merge(outdat, l2, by = id)
  
  outdat
  
}
