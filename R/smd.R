#' Standardized mean differences
#'
#' The `smd()` function provides various standardized mean effect sizes for
#' single-case data.
#'
#' @inheritParams .inheritParams
#' @details 'sd cohen' is the (unweigted) average of the variance of phase A and
#'   B. 'sd Hedges' is the weighted average of the variance of phase A and B
#'   (with a degrees of freedom correction). 'Hedges' g' is the mean difference
#'   divided by 'sd Hedges'. 'Hedges' g correction' and 'Hedges' g durlak
#'   correction' are two approaches of correcting Hedges' g for small sample
#'   sizes. 'Glass' delta' is the mean difference divided by the standard
#'   deviation of the A-phase. 'Cohens d` is the mean difference divided by 'sd
#'   cohen'.
#' @author Juergen Wilbert
#' @seealso [overlap()], [describe()]
#' @examples
#' smd(exampleAB)
#' @export
smd <- function(data, dvar, pvar, mvar, 
                decreasing = FALSE, 
                phases = c(1, 2)) {
  
  # set defaults attributes
  if (missing(dvar)) dvar <- dv(data) 
  if (missing(pvar)) pvar <- phase(data) 
  if (missing(mvar)) mvar <- mt(data) 
  
  dv(data) <- dvar
  phase(data) <- pvar
  mt(data) <- mvar
  
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
    
    A <- data[data[, pvar] == "A", dvar]
    B <- data[data[, pvar] == "B", dvar]
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
  attr(out, opt("phase")) <- source_attributes[[opt("phase")]]
  attr(out, opt("mt"))    <- source_attributes[[opt("mt")]]
  attr(out, opt("dv"))    <- source_attributes[[opt("dv")]]
  
  out
  
}
