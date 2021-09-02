#' Select and combine phases for overlap analyses
#' 
#' Useful when working with %>% operators.
#' 
#' @inheritParams .inheritParams
#' @param A Selection of the A phase
#' @param B Selection of the B phase
#'
#' @return An scdf with selected phases
#' @examples 
#' exampleA1B1A2B2 %>% 
#'   select_phases(A = c(1, 3), B = c(2, 4)) %>%
#'   overlap()
#' @export
select_phases <- function(data, A, B, ...) {
  data <- .prepare_scdf(data)
  keep <- .keep_phases(data, phases = list(A = A, B = B))
  data <- keep$data
  scdf_attr(data, "phase_selection") <- list(A = keep$phases_A, B = keep$phases_B)
  data
}

