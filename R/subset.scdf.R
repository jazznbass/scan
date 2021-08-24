#' Subset cases, rows, and variables
#'
#' @param x An scdf object.
#' @param ... [not implemented]
#' @param filter Logical expression indicating rows to keep: missing values are taken as false.
#' @param select Expression, indicating columns to select from an scdf.
#' @param cases Expression, indicating cases to keep from an scdf.
#'
#' @return An scdf.
#' @export
#'
#' @examples
#' subset(exampleAB, (values < 60 & phase == "A") | (values >= 60 & phase == "B"))
#' subset(exampleAB_add, select = c(-cigarrets, -depression))
#' subset(exampleAB, cases = c(Karolina, Johanna))
#' subset(exampleA1B1A2B2, phase %in% c("A1", "B2"), cases = Pawel:Moritz)

subset.scdf <- function(x, filter, select, cases, ...) {
  scdf <- x
  scdf_attributes <- attributes(scdf)
  if (missing(filter)) filter <- TRUE else filter <- substitute(filter)
  if (missing(select)) select <- TRUE
  if (missing(cases)) cases <- TRUE
  
  #if (is.numeric(eval(subset))) subset <- eval(subset)
   
  nl <- as.list(seq_along(scdf))
  names(nl) <- names(scdf)
  scdf <- scdf[eval(substitute(cases), envir = nl, enclos = parent.frame())]
  
   
  for(i in 1:length(scdf)) {
    x <- scdf[[i]]
    # select vars
    nl <- as.list(seq_along(x))
    names(nl) <- names(x)
    columns <- eval(substitute(select), nl, parent.frame())
    # filter rows
    if (isTRUE(filter)) rows <- TRUE
    if (is.call(filter)) {
      rows <- eval(filter, x, parent.frame())
      if (is.logical(rows))
        rows <- rows & !is.na(rows)
    }
    #print(rows)
    scdf[[i]] <- x[rows, columns, drop = FALSE]
  }
  
  scdf
}
