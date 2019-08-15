#' Select a subset of cases
#'
#' @param scdf 
#' @param ... 
#'
#' @return An scdf with a subset of cases
#' @export
#'
#' @examples
#' select_cases(exampleAB, "Johanna", "Karolina")
#' select_cases(exampleAB, 1,2)
#' select_cases(exampleAB, "-Johanna")
select_cases <- function(scdf, ...) {
  
  vars <- names(scdf)
  .eval <- function(x) {
    if (class(x) == "character") {
      weight <- ifelse(substring(x, 1, 1) == "-", -1, 1)
      x <- ifelse(substring(x, 1, 1) == "-", substring(x, 2, nchar(x)), x)
      x <- sapply(x, function(y) which(vars %in% y))
      x <- as.numeric(x)
      x <- x * weight
    }
    if (any(sapply(x, is.na))) stop("Unknown case name.")
    x

  }

  select <- as.list(substitute(list(...)))[-1]
  select <- lapply(select, function(x) 
    if (class(x) == "name") { as.character(x) 
    } else if (class(x) == "call") { eval(x) 
    } else x
  )
  
  select <- lapply(select, .eval)
  select <- unlist(select)
  scdf[select]
}

