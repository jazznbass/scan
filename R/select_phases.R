#' Select phases for overlap analyses
#' 
#' Useful when working with %>% operators.
#' 
#' @inheritParams .inheritParams
#' @param A Selection of the A phase
#' @param B Selection of the B phase
#'
#' @return An scdf with selected phases
#' @export
select_phases <- function(data, A, B, ...) {
  data <- .prepare_scdf(data)
  .keep_phases(data, phases = list(A = A, B = B))$data
  
}

