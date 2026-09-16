#' Select a subset of cases from an scdf
#' 
#' This function allows to select a subset of cases from an scdf by
#' specifying either the case names or their numeric indices. Negative
#' selection is also supported.
#'
#' @inheritParams .inheritParams
#' @param ... Selection criteria. Either numeric, objectnames, or as characters.
#' @return An scdf with a subset of cases.
#' @author Juergen Wilbert
#' @export
#' @family data manipulation functions
#' @examples
#' select_cases(exampleAB, Johanna, Karolina)
#' select_cases(exampleAB, c(Johanna, Karolina))
#' select_cases(exampleAB, 1,2)
#' select_cases(exampleAB, 1:2)
#' select_cases(exampleAB, -Johanna)
#' select_cases(exampleAB, -c(Johanna, Karolina))
#' v <- c("Moritz", "Jannis")
#' select_cases(exampleA1B1A2B2, v)
#' @export
select_cases <- function(scdf, ...) {
  cases <- as.list(substitute(list(...)))[-1]
  if (length(cases) == 0) abort("No case selected.")
  
  nl <- as.list(seq_along(scdf))
  names(nl) <- names(scdf)
  env <- parent.frame()
  
  ids <- unlist(lapply(cases, function(x) {
    id <- eval(x, envir = nl, enclos = env)
    if (is.character(id)) id <- match(id, names(scdf))
    id
  }))
  
  if (any(ids < 0, na.rm = TRUE) && any(ids > 0, na.rm = TRUE)) {
    abort("Positive and negative case selections can not be mixed.")
  }
  
  scdf[ids]
}


