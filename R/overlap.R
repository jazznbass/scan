#' Overlap indices for single-case data
#' 
#' The \code{overlap} function provides the most common overlap indices for
#' single-case data and some additional statistics.
#' 
#' @inheritParams .inheritParams
#' @details See corresponding functions of PND, PEM, PET, NAP, PAND for calculation. Tau_U reports "A vs. B + Trend B - Trend A". Base_Tau is baseline corrected tau (correction applied when autocorrelation in phase A is significant). Diff_mean is the mean difference. Diff_trend is the difference in the regression estimation of the dependent variable on measurement-time ( x ~ mt) for each phase. SMD is the mean difference divided by the standardd eviation of phase A. Hedges_g is the mean difference divided by the pooled standard deviation [sqrt(((nA - 1) * sdA^2 + (nB - 1) * sdB^2) / (nA + nB - 2) )] with a hedges correction applied [(* (1 - (3 / (4 * n - 9) ) )].
#' @return 
#' \item{overlap}{A data frame consisting of the following indices for
#' each single-case for all cases: PND, PEM, PET, NAP, PAND, Tau-U (A vs. B -
#' Trend A), Diff_mean, Diff_trend, SMD, Hedges-g.}
#' \item{phases.A}{Selection for A phase.}
#' \item{phases.B}{Selection for B phase.}
#' \item{design}{Phase design.}
#' @family overlap functions
#' @author Juergen Wilbert
#' @examples
#' 
#' ## Display overlap indices for one single-case
#' overlap(Huitema2000, decreasing = TRUE)
#' 
#' ## Display overlap indices for six single-cases
#' overlap(GruenkeWilbert2014)
#' 
#' ## Combining phases for analyszing designs with more than two phases   
#' overlap(exampleA1B1A2B2, phases = list(c("A1","A2"), c("B1","B2")))
#' 
#' @export
overlap <- function(data, dvar, pvar, mvar, 
                    decreasing = FALSE, 
                    phases = c(1, 2)){

  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) 
    dvar <- dv(data) else dv(data) <- dvar
  if (missing(pvar)) 
    pvar <- phase(data) else phase(data) <- pvar
  if (missing(mvar)) 
    mvar <- mt(data) else mt(data) <- mvar
  
  data_list <- .prepare_scdf(data)
  
  keep <- recombine_phases(data_list, phases = phases)
  data_list <- keep$data
  
  designs <- lapply(keep$designs, function(x) x$values)
  designs <- sapply(designs, function(x) paste0(x, collapse = "-"))
  
  N <- length(data_list)

  case_names <- revise_names(data_list)

  vars <- c(
    "PND", "PEM", "PET", "NAP", "NAP rescaled", "PAND", "Tau_U", 
    "Base_Tau",  "Diff_mean", "Diff_trend", "SMD", "Hedges_g"
  )
  df <- as.data.frame(matrix(nrow = N, ncol = length(vars)))
  colnames(df) <- vars
  df <- data.frame(Case = case_names, Design = designs, df, check.names = FALSE)
  
  for(i in 1:N) {
    data <- data_list[i]
    df$PND[i] <- pnd(data, decreasing = decreasing)$PND
    df$PEM[i] <- pem(data, decreasing = decreasing, binom.test = FALSE, chi.test = FALSE)$PEM
    df$PET[i] <- pet(data, decreasing = decreasing)$PET
    df$NAP[i] <- nap(data, decreasing = decreasing)$nap$NAP[1]
    df$"NAP rescaled"[i] <- nap(data, decreasing = decreasing)$nap$Rescaled[1]
    df$PAND[i] <- pand(data, decreasing = decreasing)$pand
    #df$TAU_U[i] <- tauUSC(data)$Overall_tau_u[2]
    df$Tau_U[i] <- tau_u(data)$table[[1]]["A vs. B + Trend B - Trend A", "Tau"]
    df$Base_Tau[i] <- corrected_tau(data)$tau
    
    data <- data[[1]]
    A <- data[data[, pvar] == "A", dvar]
    B <- data[data[, pvar] == "B", dvar]
    mtA <- data[data[, pvar] == "A", mvar]
    mtB <- data[data[, pvar] == "B", mvar]
    nA <- sum(!is.na(A))
    nB <- sum(!is.na(B))    
    n <- nA + nB
    mA <- mean(A, na.rm = TRUE)
    mB <- mean(B, na.rm = TRUE)    
    sdA <- sd(A, na.rm = TRUE)
    sdB <- sd(B, na.rm = TRUE)    
    
    
    df$Diff_mean[i] <- mB - mA
    df$SMD[i] <- (mB - mA) / sdA
    
    sd_hg <- sqrt(
      ( (nA - 1) * sdA^2 + (nB - 1) * sdB^2) 
      / 
      (nA + nB - 2) 
    )  
    
    df$Hedges_g[i] <- (mB - mA) / sd_hg
    df$Hedges_g[i] <- df$Hedges_g[i] * (1 - (3 / (4 * n - 9)))
    
    df$Diff_trend[i] <- coef(lm(B ~ I(mtB - mtB[1] + 1)))[2] - 
                        coef(lm(A ~ I(mtA - mtA[1] + 1)))[2]
    
  }
  
  out <- list(
    overlap = df, 
    phases.A = keep$phases_A, 
    phases.B = keep$phases_B 
    #design = keep$design[[1]]$values
  )
  
  class(out) <- c("sc_overlap")
  
  source_attributes <- attributes(data_list)[[opt("scdf")]]
  attr(out, opt("phase")) <- source_attributes[[opt("phase")]]
  attr(out, opt("mt"))    <- source_attributes[[opt("mt")]]
  attr(out, opt("dv"))    <- source_attributes[[opt("dv")]]
  
  out
}

#' @rdname deprecated-functions
#' @export
overlapSC <- function(...) {
  .deprecated_warning("overlap", "overlapSC")
  overlap(...)
}
