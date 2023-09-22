#' Add level-2 data
#'
#' Merges variables with corresponding case names from a data.frame with an scdf
#' file
#'
#' This function is mostly used in combination with the [hplm()] function.
#'
#' @inheritParams .inheritParams
#' @param data_l2 A level 2 dataset.
#' @param cvar Character string with the name of the "case" variable in the L2
#'   dataset (default is 'case').
#' @return An scdf
#' @seealso [hplm()]
#' @family data manipulation functions
#' @keywords transform
#' @examples
#' Leidig2018 %>% add_l2(Leidig2018_l2)
#' @export
add_l2 <- function(scdf,
                   data_l2,
                   cvar = "case") {

  if (is.null(names(scdf))) {
    stop("scdf must have casenames.")
  }

  if (any(is.na(names(scdf)))) {
    stop("scdf has missing casename(s).")
  }

  for (i in seq_along(scdf)) {

    casename <- names(scdf)[i]
    id <- which(data_l2[[cvar]] == casename)

    if (length(id) > 1) {
      stop("Multiple matches for a casename in the L1 dataset for ", casename)
    }

    if (length(id) == 0) {
      warning("No matching case in L2 data found for ", casename)
    }

    if (length(id) == 1) {
      scdf[[i]] <- cbind(
        scdf[[i]],
        data_l2[id, -which(names(data_l2) == cvar), drop = FALSE],
        row.names = NULL
      )
    }
  }

  scdf
}
