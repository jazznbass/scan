#' Select and combine phases for overlap analyses
#'
#' Useful when working with pipe operators. This function allows to select and
#' combine specific phases from an scdf for overlap analyses. For example, in an
#' ABAB design one might want to combine both A phases and both B phases (e.g.,
#' `A = c(1, 3)`, `B = c(2, 4)`). The resulting scdf can then be used in overlap
#' functions such as [overlap()].
#'
#' @details This function selects and combines the specified phases from the
#'   input scdf and returns the resulting scdf with the selected phases only.
#'   This is particularly useful when working with pipe operators, allowing to
#'   select and combine phases before applying overlap functions. The resulting
#'   scdf contains only the selected and combined phases, with phase labels "A"
#'   and "B". If `phase_names = "auto"`, phase names are generated from the
#'   combination of the names of the recombined phases. For example, combining
#'   phases 1 and 3 will result in the phase name "1_3". This function
#'   simplifies the process of preparing data for overlap analyses by allowing
#'   users to easily select and combine phases in a single step.
#'
#' @family overlap functions
#' @keywords overlap
#' @author Juergen Wilbert
#'
#' @inheritParams .inheritParams
#' @param A Selection of the A phase
#' @param B Selection of the B phase
#' @param phase_names A character vector with names for the resulting phases.
#'   The default `"auto"` generates phase names from the combination of the
#'   names of the recombined phases.
#' @return An scdf with selected phases
#' @examples
#' exampleA1B1A2B2_zvt |>
#'   select_phases(A = c(1, 3), B = c(2, 4)) |>
#'   overlap()
#' @export
select_phases <- function(data, A, B, phase_names = "auto") {
  data <- .prepare_scdf(data)
  keep <- recombine_phases(data, phases = list(A = A, B = B), phase_names = phase_names)
  keep$data
}

