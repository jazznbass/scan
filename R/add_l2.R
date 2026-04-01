#' Add level-2 data to an scdf
#'
#' Merges variables with corresponding case names from a data.frame with an scdf.
#'
#' This function is mostly used in combination with the [hplm()] function.
#' It adds level-2 variables to each single-case data frame in an scdf based on
#' matching case names.
#'
#' @inheritParams .inheritParams
#' @param data_l2 A level 2 dataset.
#' @param cvar Character string with the name of the "case" variable in the L2
#'   dataset (default is 'case').
#' @return An scdf with added level-2 variables.
#' @seealso [hplm()]
#' @family data manipulation functions
#' @keywords transform
#' @examples
#' ## Example with the default case variable name 'case'
#' Leidig2018 |> add_l2(Leidig2018_l2)
#' ## Example with a different case variable name in the L2 data
#' Leidig2018_l2_renamed <- Leidig2018_l2
#' names(Leidig2018_l2_renamed)[2] <- "subject"
#' Leidig2018 |> add_l2(Leidig2018_l2_renamed, cvar = "subject")
#' @author Juergen Wilbert
#' @export
add_l2 <- function(scdf,
                   data_l2,
                   cvar = "case") {
  
  check_args(
    by_class(scdf, "scdf"),
    by_class(data_l2, "data.frame"),
    by_class(cvar, "character"),
    is_true(!is.null(names(scdf)), "scdf must have casenames."),
    is_true(any(names(data_l2) == cvar), paste0("cvar '", cvar, "' not found in data_l2.")),
    is_true(!any(is.na(names(scdf))), "scdf has missing casenames.")
  )
  
  for (i in seq_along(scdf)) {

    casename <- names(scdf)[i]
    id <- which(data_l2[[cvar]] == casename)

    if (length(id) == 0) warning("No matching case in L2 data found for ", casename)
    if (length(id) > 1) stop("Multiple matches for a casename in the L1 dataset for ", casename)

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
