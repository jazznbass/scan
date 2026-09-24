#' Standardized mean differences
#'
#' Mean difference between phase A and phase B of each case, standardized in
#' five different ways.
#'
#' @details The standardisers differ in which spread they use. `sd cohen` is
#'   the root of the unweighted average of the two phase variances,
#'   \eqn{\sqrt{(sd_A^2 + sd_B^2) / 2}}. `sd hedges` weights them by their
#'   degrees of freedom, \eqn{\sqrt{((n_A - 1) sd_A^2 + (n_B - 1) sd_B^2) /
#'   (n_A + n_B - 2)}}.
#'
#'   `Cohen's d` is the mean difference divided by `sd cohen`, `Hedges' g`
#'   divides it by `sd hedges`, and `Glass' delta` by the standard deviation of
#'   phase A alone. `Hedges' g correction` and `Hedges' g durlak correction`
#'   are two ways of correcting `Hedges' g` for small samples: the first
#'   multiplies it by \eqn{1 - 3 / (4n - 9)}, the second by \eqn{(n - 3) /
#'   (n - 2.25) \sqrt{(n - 2) / n}}, with \eqn{n} the number of observed
#'   measurements in both phases.
#'
#'   Missing values are dropped, and the counts \eqn{n_A} and \eqn{n_B} refer
#'   to the observed measurements. The formulas are evaluated as they are, so a
#'   phase with fewer than two observed measurements gives `NA` rather than an
#'   error.
#' @inheritParams .inheritParams
#' @return An object of class `sc_smd` with the element `smd`, a data frame
#'   holding one row per case:
#'  |  |  |
#'  | --- | --- |
#'  | `Case` | Name of the case. |
#'  | `mA`, `mB` | Mean of phase A and of phase B. |
#'  | `sdA`, `sdB` | Standard deviation of phase A and of phase B. |
#'  | `sd cohen`, `sd hedges` | The two standardisers described above. |
#'  | `Glass' delta` | Mean difference divided by `sdA`. |
#'  | `Hedges' g` | Mean difference divided by `sd hedges`. |
#'  | `Hedges' g correction` | `Hedges' g` corrected for small samples. |
#'  | `Hedges' g durlak correction` | `Hedges' g` with Durlak's correction. |
#'  | `Cohen's d` | Mean difference divided by `sd cohen`. |
#' @author Juergen Wilbert
#' @seealso [overlap()], [describe()], [between_smd()]
#' @examples
#' smd(exampleAB)
#'
#' # pooling the two A and the two B phases of an ABAB design
#' smd(exampleABAB, phases = list(A = c(1, 3), B = c(2, 4)))
#' @order 1
#' @export
smd <- function(data, dvar, pvar,
                phases = c(1, 2)) {
  
  # set defaults attributes
  if (missing(dvar)) dvar <- dv(data) else dv(data) <- dvar
  if (missing(pvar)) pvar <- phase(data) else phase(data) <- pvar
  
  data_list <- .prepare_scdf(data)
  
  keep <- recombine_phases(data_list, phases = phases)
  data_list <- keep$data
  
  N <- length(data_list)
  
  case_names <- revise_names(names(data_list), length(data_list))
  
  vars <- c(
    "mA", "mB", "sdA", "sdB", "sd cohen", "sd hedges", "Glass' delta",  
    "Hedges' g", "Hedges' g correction",
    "Hedges' g durlak correction", "Cohen's d"
  )
  df <- as.data.frame(matrix(nrow = N, ncol = length(vars)))
  colnames(df) <- vars
  df <- data.frame(Case = case_names, df, check.names = FALSE)
  
  for(i in 1:N) {
    data <- data_list[i][[1]]
    
    A <- data[data[[pvar]] == "A", dvar]
    B <- data[data[[pvar]] == "B", dvar]
    nA <- sum(!is.na(A))
    nB <- sum(!is.na(B))    
    n <- nA + nB
    mA <- mean(A, na.rm = TRUE)
    mB <- mean(B, na.rm = TRUE)    
    sdA <- sd(A, na.rm = TRUE)
    sdB <- sd(B, na.rm = TRUE) 
    
    df$mA[i] <- mA
    df$mB[i] <- mB
    df$sdA[i] <- sdA
    df$sdB[i] <- sdB
    
    df$"Glass' delta"[i] <- (mB - mA) / sdA
    
    df$"sd hedges"[i] <- sqrt(
      ((nA - 1) * sdA^2 + (nB - 1) * sdB^2) / (nA + nB - 2) 
    )  
    
    df$"Hedges' g"[i] <- (mB - mA) / df$"sd hedges"[i]
    
    corr_hedges <- 1 - (3 / (4 * n - 9))
    df$"Hedges' g correction"[i] <- df$"Hedges' g"[i] * corr_hedges
    
    corr_durlak <- (n - 3) / (n - 2.25) * sqrt((n - 2) / n)
    
    df$"Hedges' g durlak correction"[i] <- df$"Hedges' g"[i] * corr_durlak
    
    df$"sd cohen"[i] <- sqrt((sdA^2 + sdB^2) / 2)
    df$"Cohen's d"[i] <- (mB - mA) / df$"sd cohen"[i]
    
  }
  
  out <- list(
    smd = df, 
    phases.A = keep$phases_A, 
    phases.B = keep$phases_B 
  )
  
  class(out) <- c("sc_smd")
  
  source_attributes <- attributes(data_list)[[opt("scdf")]]
  nm <- opts("phase", "dv")
  attributes(out)[nm] <- source_attributes[nm]
  
  out
  
}
