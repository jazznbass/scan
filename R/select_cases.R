#' Select a subset of cases
#'
#' @inheritParams .inheritParams
#' @param ... Selection criteria. Either numeric, objectnames, or as characters.
#'
#' @return An scdf with a subset of cases
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
  
  nl <- as.list(seq_along(scdf))
  names(nl) <- names(scdf)
  
  selection <- lapply(cases, function(x) scdf[eval(x, envir = nl, enclos = parent.frame())])
  
  out <- selection[[1]]
  if (length(selection) > 1) 
    for(i in 2:length(selection)) out <- c(out, selection[[i]])
  out
}

