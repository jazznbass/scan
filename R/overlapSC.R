#' Overlap indices for single-case data
#' 
#' The \code{overlapSC} function provides the most common overlap indices for
#' single-case data and some additional statistics.
#' 
#' @inheritParams .inheritParams
#' @return 
#' \item{overlap}{A data frame consisting of the following indices for
#' each single-case for all cases: PND, PEM, PET, NAP, PAND, Tau-U (A vs. B -
#' Trend A), Diff_mean, Diff_trend, SMD.}
#' \item{phases.A}{Selection for A phase.}
#' \item{phases.B}{Selection for B phase.}
#' \item{design}{Phase design.}
#' @family overlap functions
#' @author Juergen Wilbert
#' @examples
#' 
#' ## Display overlap indices for one single-case
#' overlapSC(Huitema2000, decreasing = TRUE)
#' 
#' ## Display overlap indices for six single-cases
#' overlapSC(GruenkeWilbert2014)
#' 
#' ## Combining phases for analyszing designs with more than two phases   
#' overlapSC(exampleA1B1A2B2, phases = list(c("A1","A2"), c("B1","B2")))
#' 
#' @export
overlapSC <- function(data, dvar, pvar, mvar, decreasing = FALSE, phases = c(1, 2)) {

  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- scdf_attr(data, .opt$dv) else scdf_attr(data, .opt$dv) <- dvar
  if (missing(pvar)) pvar <- scdf_attr(data, .opt$phase) else scdf_attr(data, .opt$phase) <- pvar
  if (missing(mvar)) mvar <- scdf_attr(data, .opt$mt) else scdf_attr(data, .opt$mt) <- mvar
  
  data.list <- .SCprepareData(data)
  keep <- .keepphasesSC(data.list, phases = phases, pvar = pvar)
  data.list <- keep$data
  
  design <- rle(as.character(data.list[[1]][, pvar]))$values
  N <- length(data.list)

  case.names <- names(data.list)

  VAR <- c(
    "PND", "PEM", "PET", "NAP", "NAP.rescaled", "PAND", "TAU_U", 
    "Base_Tau",  "Diff_mean", "Diff_trend","SMD"
  )
  d.f <- as.data.frame(matrix(nrow = N, ncol = length(VAR)))
  colnames(d.f) <- VAR
  rownames(d.f) <- c(case.names)
  
  for(i in 1:N) {
    data <- data.list[i]
    d.f$PND[i] <- pnd(data, decreasing = decreasing)$PND
    d.f$PEM[i] <- pem(data, decreasing = decreasing, binom.test = FALSE, chi.test = FALSE)$PEM
    d.f$PET[i] <- pet(data, decreasing = decreasing)$PET
    d.f$NAP[i] <- nap(data, decreasing = decreasing)$nap$NAP[1]
    d.f$NAP.rescaled[i] <- nap(data, decreasing = decreasing)$nap$Rescaled[1]
    d.f$PAND[i] <- pand(data, decreasing = decreasing)$PAND
    #d.f$TAU_U[i] <- tauUSC(data)$Overall_tau_u[2]
    d.f$TAU_U[i] <- tauUSC(data)$table[[1]]["A vs. B + Trend B - Trend A", "Tau"]
    d.f$Base_Tau[i] <- corrected_tauSC(data)$tau
    
    data <- data[[1]]
    A <- data[data[, pvar] == "A", dvar]
    B <- data[data[, pvar] == "B", dvar]
    d.f$Diff_mean[i] <- mean(B, na.rm = TRUE) - mean(A, na.rm = TRUE)
    d.f$SMD[i] <- (mean(B, na.rm = TRUE) - mean(A, na.rm = TRUE)) / sd(A, na.rm = TRUE)
    
    A.MT <- data[data[, pvar] == "A", mvar]
    B.MT <- data[data[, pvar] == "B", mvar]
    d.f$Diff_trend[i] <- coef(lm(B ~ I(B.MT - B.MT[1] + 1)))[2] - coef(lm(A ~ I(A.MT - A.MT[1] + 1)))[2]
    
  }
  
  out <- list(overlap = d.f, phases.A = keep$phases.A, phases.B = keep$phases.B, design = keep$design[[1]]$values)
  class(out) <- c("sc", "overlap")
  ATTRIBUTES <- attributes(data.list)[[.opt$scdf]]
  attr(out, .opt$phase) <- ATTRIBUTES[[.opt$phase]]
  attr(out, .opt$mt)    <- ATTRIBUTES[[.opt$mt]]
  attr(out, .opt$dv)    <- ATTRIBUTES[[.opt$dv]]
  
  out
}
