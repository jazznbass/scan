#' Select and combine phases for overlap analyses
#'
#' Useful when working with pipe operators.
#'
#' @inheritParams .inheritParams
#' @param A Selection of the A phase
#' @param B Selection of the B phase
#' @param phase_names A character vector with names for the resulting phases.
#'   The default `"auto"` generates phase names from the combination of the names
#'   of the recombined phases.
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

