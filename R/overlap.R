#' Overlap indices for single-case data
#' 
#' The \code{overlap} function provides the most common overlap indices for
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
                    phases = c(1, 2),
                    pnd = TRUE,
                    pem = TRUE,
                    pet = TRUE,
                    nap = TRUE,
                    nap_r = TRUE,
                    pand = TRUE,
                    tau_u = TRUE,
                    base_tau = TRUE,
                    diff_mean = TRUE,
                    diff_trend = TRUE,
                    smd = TRUE,
                    hedges_g = TRUE) {

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

  VAR <- c()
  
  if (pnd) VAR <- c(VAR, "PND")
  if (pem) VAR <- c(VAR, "PEM")
  if (pet) VAR <- c(VAR, "PET")
  if (nap) VAR <- c(VAR, "NAP")
  if (nap_r) VAR <- c(VAR, "NAP_rescaled")
  if (pand) VAR <- c(VAR, "PAND")
  if (tau_u) VAR <- c(VAR, "Tau_U")
  if (base_tau) VAR <- c(VAR, "Base_Tau")
  if (diff_mean) VAR <- c(VAR, "Diff_mean")
  if (diff_trend) VAR <- c(VAR, "Diff_trend")
  if (smd) VAR <- c(VAR, "SMD")
  if (hedges_g) VAR <- c(VAR, "Hedges_g")

  d.f <- as.data.frame(matrix(nrow = N, ncol = length(VAR)))
  colnames(d.f) <- VAR
  rownames(d.f) <- c(case.names)
  
  for(i in 1:N) {
    data <- data.list[i]
    
    if (pnd) 
      d.f$PND[i] <- pnd(data, decreasing = decreasing)$PND
    if (pem) 
      d.f$PEM[i] <- pem(data, decreasing = decreasing, binom.test = FALSE, chi.test = FALSE)$PEM
    if (pet)
      d.f$PET[i] <- pet(data, decreasing = decreasing)$PET
    if (nap)
      d.f$NAP[i] <- nap(data, decreasing = decreasing)$nap$NAP[1]
    if (nap_r)
      d.f$NAP_rescaled[i] <- nap(data, decreasing = decreasing)$nap$Rescaled[1]
    if (pand)
      d.f$PAND[i] <- pand(data, decreasing = decreasing)$PAND
    #d.f$TAU_U[i] <- tauUSC(data)$Overall_tau_u[2]
    if (tau_u)
      d.f$Tau_U[i] <- tau_u(data)$table[[1]]["A vs. B + Trend B - Trend A", "Tau"]
    if (base_tau)
      d.f$Base_Tau[i] <- corrected_tau(data)$tau
    
    data <- data[[1]]
    A <- data[data[, pvar] == "A", dvar]
    B <- data[data[, pvar] == "B", dvar]
    A.MT <- data[data[, pvar] == "A", mvar]
    B.MT <- data[data[, pvar] == "B", mvar]
    n <- sum(!is.na(c(A, B)))
    
    if (diff_mean)
      d.f$Diff_mean[i] <- mean(B, na.rm = TRUE) - mean(A, na.rm = TRUE)
    if (smd)
      d.f$SMD[i] <- (mean(B, na.rm = TRUE) - mean(A, na.rm = TRUE)) / sd(A, na.rm = TRUE)
    
    if (hedges_g)
      d.f$Hedges_g[i] <- ((mean(B, na.rm = TRUE) - mean(A, na.rm = TRUE)) / sd(A, na.rm = TRUE)) * (1 - (3 / (4 * n - 9)))
    
    if (diff_trend)
      d.f$Diff_trend[i] <- coef(lm(B ~ I(B.MT - B.MT[1] + 1)))[2] - coef(lm(A ~ I(A.MT - A.MT[1] + 1)))[2]
    
  }
  
  out <- list(
    overlap = d.f, 
    phases.A = keep$phases.A, 
    phases.B = keep$phases.B, 
    design = keep$design[[1]]$values
  )
  
  class(out) <- c("sc", "overlap")
  
  ATTRIBUTES <- attributes(data.list)[[.opt$scdf]]
  attr(out, .opt$phase) <- ATTRIBUTES[[.opt$phase]]
  attr(out, .opt$mt)    <- ATTRIBUTES[[.opt$mt]]
  attr(out, .opt$dv)    <- ATTRIBUTES[[.opt$dv]]
  
  out
}

#' @rdname overlap
#' @export
overlapSC <- function(...) {
  .deprecated_warning("overlap", "overlapSC")
  overlap(...)
}