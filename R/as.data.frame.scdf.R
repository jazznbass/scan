#' Creating a long format data frame from several single-case data frames
#' (scdf).
#'
#' The as.data.frame function transposes an scdf into one long data frame.
#' Additionally, a data frame can be merged that includes level 2 data of the
#' subjects. This might be helpful to prepare data to be used with other
#' packages than scan.
#'
#'
#' @param x An scdf object
#' @param l2 A data frame providing additional variables at Level 2. The scdf
#'   has to have names for all cases and the Level 2 data frame has to have a
#'   column with corresponding case names.
#' @param id Variable name of the Level 2 data frame that contains the case
#'   names.
#' @param ... Not implemented
#' @return Returns one data frame with data of all single-cases structured by
#'   the case variable.
#' @author Juergen Wilbert
#' @family data manipulation functions
#' @keywords manip
#' @examples
#'
#' ## Convert the list of three single-case data frames from Grosche (2011)
#' ### into one long data frame
#' Grosche2011
#' Grosche2011_long <- as.data.frame(Grosche2011)
#' Grosche2011_long
#'
#' ## Combine an scdf with data for l2
#' Leidig2018_long <- as.data.frame(Leidig2018, l2 = Leidig2018_l2)
#' names(Leidig2018_long)
#' summary(Leidig2018_long)
#'
#' @export
as.data.frame.scdf <- function(x, ..., l2 = NULL, id = "case") {
  if (!is.null(l2)) x <- add_l2(x, l2)

  label <- revise_names(x)
  outdat <- vector()

  for (i_case in seq_along(x)) {
    x[[i_case]]$case <- label[i_case]
    outdat <- rbind(outdat, x[[i_case]])
  }

  outdat <- cbind(outdat[, ncol(outdat)], outdat[, -ncol(outdat)])
  colnames(outdat)[1] <- id
  outdat[[1]] <- factor(outdat[[1]], levels = label, labels = label)
  attr(outdat, "scdf") <- attr(x, "scdf")
  outdat
}
