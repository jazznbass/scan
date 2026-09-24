#' Overlap indices
#'
#' The most common overlap indices and some additional effect sizes for each
#' case of a single-case data set, gathered in one table.
#'
#' @details Each index is taken from the function of the same name, called
#'   per case with the `decreasing` argument passed on: [pnd()], [pem()],
#'   [pet()], [nap()], [pand()] (with `method = "sort"`), [ird()], [tau_u()]
#'   and [corrected_tau()]. `Tau_U(A)` is the model `A vs. B - Trend A`,
#'   `Tau_U(BA)` the model `A vs. B + Trend B - Trend A`, and `Base_Tau` the
#'   baseline corrected tau. Neither of the three tau columns takes
#'   `decreasing` into account, as their sign already carries the direction.
#'
#'   The remaining four columns are computed here. `Diff_mean` is the mean of
#'   phase B minus the mean of phase A. `Diff_trend` is the difference of the
#'   regression weights of the dependent variable on the measurement time,
#'   estimated separately within each phase. `SMD` is the mean difference
#'   divided by the standard deviation of phase A, which is what [smd()] calls
#'   `Glass' delta`. `Hedges_g` is the mean difference divided by the pooled
#'   standard deviation \eqn{\sqrt{ (n_A - 1)sd_A^2 + (n_B - 1)sd_B^2 \over n_A
#'   + n_B - 2 }} and always carries the small sample correction \eqn{1 -
#'   \frac{3}{4n - 9}}, so it corresponds to the column `Hedges' g correction`
#'   of [smd()] and not to its `Hedges' g`.
#'
#'   Missing values are dropped, so \eqn{n_A} and \eqn{n_B} are the numbers of
#'   observed measurements. How an empty or very short phase is handled differs
#'   between the indices; see the respective function.
#' @inheritParams .inheritParams
#' @return An object of class `sc_overlap` with the element `overlap`, a data
#'   frame holding one row per case:
#'  |  |  |
#'  | --- | --- |
#'  | `Case` | Name of the case. |
#'  | `Design` | Phase design of the case after recombining phases. |
#'  | `PND` | Percentage of non-overlapping data. |
#'  | `PEM` | Percentage of data exceeding the median. |
#'  | `PET` | Percentage of data exceeding a median trend. |
#'  | `NAP`, `NAP rescaled` | Nonoverlap of all pairs, and the same rescaled to -100 to 100. |
#'  | `PAND` | Percentage of all non-overlapping data. |
#'  | `IRD` | Robust improvement rate difference. |
#'  | `Tau_U(A)`, `Tau_U(BA)` | Tau-U of the two models named above. |
#'  | `Base_Tau` | Baseline corrected tau. |
#'  | `Diff_mean` | Mean difference between the phases. |
#'  | `Diff_trend` | Difference of the within-phase trends. |
#'  | `SMD` | Mean difference divided by the standard deviation of phase A. |
#'  | `Hedges_g` | Mean difference divided by the pooled standard deviation, small sample corrected. |
#' @author Juergen Wilbert
#' @family overlap functions
#' @seealso [corrected_tau()], [smd()]
#' @examples
#' overlap(exampleAB)
#'
#' # data that are expected to decrease in phase B
#' overlap(exampleAB_decreasing, decreasing = TRUE)
#'
#' # combining phases of a design with more than two phases
#' overlap(exampleA1B1A2B2, phases = list(c("A1", "A2"), c("B1", "B2")))
#' @order 1
#' @export
overlap <- function(data, dvar, pvar, mvar, 
                    decreasing = FALSE, 
                    phases = c(1, 2)){

  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- dv(data) else dv(data) <- dvar
  if (missing(pvar)) pvar <- phase(data) else phase(data) <- pvar
  if (missing(mvar)) mvar <- mt(data) else mt(data) <- mvar
  
  data_list <- .prepare_scdf(data)
  
  keep <- recombine_phases(data_list, phases = phases)
  data_list <- keep$data
  
  designs <- lapply(keep$designs, function(x) x$values)
  designs <- sapply(designs, function(x) paste0(x, collapse = "-"))
  
  N <- length(data_list)

  case_names <- revise_names(data_list)

  vars <- c(
    "PND", "PEM", "PET", "NAP", "NAP rescaled", "PAND", "IRD", "Tau_U(A)", 
    "Tau_U(BA)", "Base_Tau",  "Diff_mean", "Diff_trend", "SMD", "Hedges_g"
  )
  df <- as.data.frame(matrix(nrow = N, ncol = length(vars)))
  colnames(df) <- vars
  df <- data.frame(Case = case_names, Design = designs, df, check.names = FALSE)
  
  for(i in 1:N) {
    data <- data_list[i]
    df$PND[i] <- pnd(data, decreasing = decreasing)$PND
    df$PEM[i] <- pem(data, 
      decreasing = decreasing, binom.test = FALSE, chi.test = FALSE)$PEM$PEM
    df$PET[i] <- pet(data, decreasing = decreasing)$PET$PET
    df$NAP[i] <- nap(data, decreasing = decreasing)$nap[[1, "NAP"]]
    df$"NAP rescaled"[i] <- nap(
      data, decreasing = decreasing)$nap[[1, "NAP Rescaled"]]
    df$PAND[i] <- pand(data, decreasing = decreasing)$pand
    df$IRD[i] <- ird(data, decreasing = decreasing)$ird
    df$`Tau_U(A)`[i] <- tau_u(data)$table[[1]]["A vs. B - Trend A", "Tau"]
    df$`Tau_U(BA)`[i] <- tau_u(data)$table[[1]]["A vs. B + Trend B - Trend A", "Tau"]
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
    
    df$Diff_trend[i] <- 
      coef(lm(B ~ I(mtB - mtB[1] + 1), na.action = na.omit))[2] - 
      coef(lm(A ~ I(mtA - mtA[1] + 1), na.action = na.omit))[2]
    
  }
  
  out <- list(
    overlap = df, 
    phases.A = keep$phases_A, 
    phases.B = keep$phases_B 
    #design = keep$design[[1]]$values
  )
  
  atr <- scdf_attr(data_list)
  for(i in seq_along(atr)) attr(out, names(atr)[i]) <- atr[[i]]
  class(out) <- c("sc_overlap")
  
  out
}

