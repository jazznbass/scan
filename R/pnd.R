#' Percentage of non-overlapping data (PND)
#'
#' Proportion of the phase B measurements that exceed the most extreme
#' measurement of phase A. PND is error-prone; [nap()] and [pand()] are the
#' better choice.
#'
#' @details Counted are the measurements of phase B above the highest value of
#'   phase A, or below the lowest for `decreasing = TRUE`, divided by the number
#'   of observed measurements in phase B. A measurement equal to the extreme
#'   value of phase A does not count. Missing values are dropped beforehand, so
#'   they are part of neither the count nor the divisor. A case without observed
#'   values in one of the phases gives `NA`.
#' @inheritParams .inheritParams
#' @return An object of class `sc_pnd` with the elements:
#'  |  |  |
#'  | --- | --- |
#'  | `PND` | Percentage of non-overlapping data per case. |
#'  | `case.names` | Names of the cases. |
#'  | `n.B` | Number of observed measurements in phase B. |
#' @author Juergen Wilbert
#' @family overlap functions
#' @references Scruggs, T. E., Mastropieri, M. A., & Casto, G. (1987). The
#'   quantitative synthesis of single-subject research: Methodology and
#'   validation. *Remedial and Special Education, 8*(2), 24-33.
#'
#'   Parker, R. I., & Vannest, K. (2009). An improved effect size for
#'   single-case research: Nonoverlap of all pairs. *Behavior Therapy, 40*(4),
#'   357-367.
#' @examples
#' pnd(exampleAB)
#'
#' # data that are expected to decrease in phase B
#' pnd(exampleAB_decreasing, decreasing = TRUE)
#' @order 1
#' @export
pnd <- function(data, dvar, pvar, decreasing = FALSE, phases = c(1, 2)) {

  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- dv(data) else dv(data) <- dvar
  if (missing(pvar)) pvar <- phase(data) else phase(data) <- pvar
  
  data <- .prepare_scdf(data)
  data <- recombine_phases(data, phases = phases)$data
  
  pnd <- c()
  n.B <- c()
  
  for(i in 1:length(data)) {
    A <- data[[i]][, dvar][data[[i]][, pvar] == "A"]
    B <- data[[i]][, dvar][data[[i]][, pvar] == "B"]
    A <- A[!is.na(A)]
    B <- B[!is.na(B)]
    n.B[i] <- length(B)
    if (length(A) == 0L || length(B) == 0L) {
      pnd[i] <- NA_real_
      next
    }
    if (!decreasing) pnd[i] <- sum(B > max(A)) /  n.B[i] * 100
    if (decreasing) pnd[i] <- sum(B < min(A)) /  n.B[i] * 100
  }
  
  out <- list(PND = pnd, case.names = names(data), n.B = n.B)
  class(out) <- c("sc_pnd")
  attributes(out)[opts("phase", "dv")] <- list(pvar, dvar)
  out
}
