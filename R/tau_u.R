#' Tau-U for single-case data
#'
#' This function calculates indices of the Tau-U family as proposed by Parker et
#' al. (2011a).
#'
#' @order 1
#' @inheritParams .inheritParams
#' @param method `"complete"` (default), `"parker"` or `"tarlow"`. The
#'   `"parker"` calculates the number of possible pairs as described in Parker
#'   et al. (2011) which might lead to tau-U values greater than 1. `"tarlow"`
#'   follows an online calculator and R code developed by Tarlow (2017).
#' @param meta_analyses If TRUE, a meta analysis is conducted.
#' @param meta_weight_method String to specify the method for calculating the
#'   weights of the studies. Either "tau" or "z".
#' @param ci Confidence intervals
#' @param ci_method String to specify the method for calculating the standard
#'   error of tau. Either "tau", "z", or "s" (not recommended).
#' @param tau_method Character with values "a" or "b" (default) indicating
#'   whether Kendall Tau A or Kendall Tau B is applied. Ignored for methods
#'   'tarlow' and 'parker'.
#' @param continuity_correction If TRUE, a continuity correction is applied for
#'   calculating p-values of correlations (here: S will be reduced by one before
#'   calculating Z). Ignored for methods 'tarlow' and 'parker'.
#' @return
#' \item{table}{A data frame containing statistics from the Tau-U
#' family, including: Pairs, positive and negative comparisons, S, and Tau}
#' \item{matrix}{The matrix of comparisons used for calculating the
#' statistics.} \item{tau_u}{Tau-U value.}
#' @details Tau-U is an inconsistently operationalized construct. Parker et al.
#'   (2011b) describe a method which may result in Tau-U outside the \[-1;1\]
#'   interval. A different implementation of the method (provided at
#'   http://www.singlecaseresearch.org/calculators/tau-u) uses tau-b (instead of
#'   tau-a as in the original formulation by Parker). Bossart et. al (2018)
#'   describe inconsistencies in the results from this implementation as well.
#'   Another problems lies in the calculation in overall Tau-U values from
#'   several single cases. The function presented here applies a meta-analysis
#'   to gain the overall values. Each tau value is weighted by the inverse of
#'   the variance (ie. the tau standard error). The confidence intervals for
#'   single cases are calculated by Fisher-Z transforming tau, calculating the
#'   confidence intervals, and inverse transform them back to tau (see Long &
#'   Cliff, 1997).
#' @author Juergen Wilbert
#' @family overlap functions
#' @references Brossart, D. F., Laird, V. C., & Armstrong, T. W. (2018).
#'   Interpreting Kendall’s Tau and Tau-U for single-case experimental designs.
#'   \emph{Cogent Psychology, 5(1)}, 1–26.
#'   https://doi.org/10.1080/23311908.2018.1518687.
#'
#'   Long, J. D., & Cliff, N. (1997). Confidence intervals for Kendall’s tau.
#'   \emph{British Journal of Mathematical and Statistical Psychology}, 50(1),
#'   31–41. https://doi.org/10.1111/j.2044-8317.1997.tb01100.x
#'
#'   Parker, R. I., Vannest, K. J., & Davis, J. L. (2011a). Effect Size in
#'   Single-Case Research: A Review of Nine Nonoverlap Techniques.
#'   \emph{Behavior Modification}, 35(4), 303–322. https://doi.org/10/dsdfs4
#'
#'   Parker, R. I., Vannest, K. J., Davis, J. L., & Sauber, S. B. (2011b).
#'   Combining Nonoverlap and Trend for Single-Case Research: Tau-U.
#'   \emph{Behavior Therapy, 42}(2), 284–299.
#'   https://doi.org/10.1016/j.beth.2010.08.006
#'
#'   Tarlow, K. R. (2017, March). Tau-U for single-case research (R code).
#'   Retrieved from http://ktarlow.com/stats/
#'
#' @examples
#'
#' tau_u(Grosche2011$Eva)
#'
#' ## Replicate  tau-U calculation from Parker et al. (2011)
#' bob <- scdf(c(A = 2, 3, 5, 3, B = 4, 5, 5, 7, 6), name = "Bob")
#' res <- tau_u(bob, method = "parker", tau_method = "a")
#' print(res, complete = TRUE)
#'
#' ## Request tau-U for all single-cases from the Grosche2011 data set
#' tau_u(Grosche2011)
#' @export

tau_u <- function(data, dvar, pvar, 
                  method = c("complete", "parker", "tarlow"), 
                  phases = c(1, 2), 
                  meta_analyses = TRUE,
                  ci = 0.95,
                  ci_method = c("z", "tau", "s"),
                  meta_weight_method = c("z", "tau"),
                  tau_method = c("b", "a"), 
                  continuity_correction = FALSE) {

  # validity check ----
  check_args(
    by_call(tau_method, "tau_u"),
    by_call(ci_method, "tau_u"),
    by_call(method, "tau_u"),
    by_call(meta_weight_method, "tau_u"),
    within(ci, 0, 1)
  )
  
  method <- method[1]
  tau_method <- tau_method[1]
  meta_weight_method <- meta_weight_method[1]
  ci_method <- ci_method[1]
  
  if (method == "parker") {
    #message("method = 'parker' ignores the tau_method argument.")
    tau_method <- "a"
    continuity_correction <- FALSE
  }
  
  if (method == "tarlow") {
    #message("method = 'tarlow' ignores the tau_method argument.")
    tau_method <- "a"
    continuity_correction <- TRUE
  }

  # prepare scdf ----
  if (missing(dvar)) dvar <- dv(data)
  if (missing(pvar)) pvar <- phase(data)
  phase(data) <- pvar
  dv(data) <- dvar
  
  data <- .prepare_scdf(data)
  data <- recombine_phases(data, phases = phases)$data
  
  # define "out" data structure ----
  N <- length(data)
  out <- list(
    table = list(),
    tau_u = list(),
    method = method,
    tau_method = tau_method,
    phases = phases,
    n_cases = N,
    continuity_correction = continuity_correction,
    Overall_tau_u = NA,
    meta_analyses = meta_analyses,
    meta_weight_method = meta_weight_method,
    ci = ci,
    ci_method = ci_method
  )
  
  # define tau table data structure -----
  row_names <- c(
    "A vs. B", 
    "Trend A", 
    "Trend B", 
    "A vs. B - Trend A",
    "A vs. B + Trend B", 
    "A vs. B + Trend B - Trend A"
  )
  col_names <- c(
    "pairs", "pos", "neg", "ties", "S", "D", "Tau", "CI lower", "CI upper",
    "SD_S", "VAR_S", "SE_Tau", "Z", "p"
  )
  
  template_table_tau <- as.data.frame(matrix(
    NA, length(row_names), length(col_names), 
    dimnames = list(row_names, col_names)
  ))

  # tau-U for each case -----
  for (case in 1:N) {
    
    table_tau <- template_table_tau
    
    # Extract A and B phase values
    .isA <- data[[case]][[pvar]] == "A"
    .isB <- data[[case]][[pvar]] == "B"
    A <- data[[case]][.isA, dvar]
    B <- data[[case]][.isB, dvar]
    
    # drop NA
    A <- A[!is.na(A)]
    B <- B[!is.na(B)]
    
    #
    AB <- c(A, B)
    nA <- length(A)
    nB <- length(B)
    nAB <- nA + nB
    
    # create tau matrix -----------------------------------------------------
    AvApos <- 0
    AvAneg <- 0
    AvAtie <- 0
    BvBpos <- 0
    BvBneg <- 0
    BvBtie <- 0
    AvBpos <- 0
    AvBneg <- 0
    AvBtie <- 0
    
    for(i in 1:(nA-1)) {
      AvApos <- AvApos + sum(A[i] < A[(i+1):nA])
      AvAneg <- AvAneg + sum(A[i] > A[(i+1):nA])
      AvAtie <- AvAtie + sum(A[i] == A[(i+1):nA])
    }
    
    for(i in 1:(nB-1)) {
      BvBpos <- BvBpos + sum(B[i] < B[(i+1):nB])
      BvBneg <- BvBneg + sum(B[i] > B[(i+1):nB])
      BvBtie <- BvBtie + sum(B[i] == B[(i+1):nB])
    }
    
    AvBpos <- sum(vapply(A, function(x) x < B, FUN.VALUE = logical(nB)))
    AvBneg <- sum(vapply(A, function(x) x > B, FUN.VALUE = logical(nB)))
    AvBtie <- sum(vapply(A, function(x) x == B, FUN.VALUE = logical(nB)))
    
    # Kendall tau analyses ----------------------------------------------------

    if (method == "complete" && tau_method == "a") {
      tau_s <- list(
        AvB = kendall_tau(AB, c(rep(0, nA), rep(1, nB)), tau_method = "a", continuity_correction = continuity_correction), 
        AvA = kendall_tau(A, 1:nA, tau_method = "a", continuity_correction = continuity_correction), 
        BvB = kendall_tau(B, 1:nB, tau_method = "a", continuity_correction = continuity_correction), 
        AvB_A = kendall_tau(AB, c(nA:1, rep(nA + 1, nB)), tau_method = "a", continuity_correction = continuity_correction), 
        AvB_B = kendall_tau(AB, c(rep(0, nA), (nA + 1):nAB), tau_method = "a", continuity_correction = continuity_correction), 
        AvB_B_A = kendall_tau(AB, c(nA:1, (nA + 1):nAB), tau_method = "a", continuity_correction = continuity_correction)
      )
    } else {
      tau_s <- list(
        AvB = kendall_tau(AB, c(rep(0, nA), rep(1, nB)), tau_method = "b", continuity_correction = continuity_correction), 
        AvA = kendall_tau(A, 1:nA, tau_method = "b", continuity_correction = continuity_correction), 
        BvB = kendall_tau(B, 1:nB, tau_method = "b", continuity_correction = continuity_correction), 
        AvB_A = kendall_tau(AB, c(nA:1, rep(nA + 1, nB)), tau_method = "b", continuity_correction = continuity_correction), 
        AvB_B = kendall_tau(AB, c(rep(0, nA), (nA + 1):nAB), tau_method = "b", continuity_correction = continuity_correction), 
        AvB_B_A = kendall_tau(AB, c(nA:1, (nA + 1):nAB), tau_method = "b", continuity_correction = continuity_correction)
      )
    }

    # n ----------------------------
    
    table_tau$n <- lapply(tau_s, \(.) .$N)|>unlist()
    
    # pairs -------------------------------------------------------------------
    
    AvB_pair <- nA * nB
    AvA_pair <- nA * (nA - 1) / 2
    BvB_pair <- nB * (nB - 1) / 2
    ABvAB_pair <- nAB * (nAB - 1) / 2
    
    table_tau$pairs <- c(
      AvB_pair, # A vs. B
      AvA_pair, # A vs. A
      BvB_pair, # B vs. B
      AvB_pair + AvA_pair, # A vs. B - A vs. A
      AvB_pair + BvB_pair, # A vs. B + B vs. B
      AvB_pair + AvA_pair + BvB_pair # A vs. B + B vs. B - A vs. A
    )
    
    if (method == "parker") {
      table_tau$pairs[4] <- AvB_pair # A vs. B - A vs. A
      table_tau$pairs[6] <- ABvAB_pair # A vs. B + B vs. B - A vs. A
    }
    
    # pos/neg/ties ------------------------------------------------------------
    
    table_tau$pos <- c(
      AvBpos,
      AvApos,
      BvBpos,
      AvBpos + AvAneg,
      AvBpos + BvBpos,
      AvBpos + BvBpos + AvAneg
    )
    
    table_tau$neg <- c(
      AvBneg,
      AvAneg,
      BvBneg,
      AvBneg + AvApos,
      AvBneg + BvBneg,
      AvBneg + BvBneg + AvApos
    )
    
    table_tau$ties <- c(
      AvBtie,
      AvAtie,
      BvBtie,
      AvBtie + AvAtie,
      AvBtie + BvBtie,
      AvBtie + BvBtie + AvAtie
    )
    
    # S -----------------------------------------------------------------
    
    table_tau$S <- lapply(tau_s, \(.) .$S)|>unlist()
   
    # D ----------------------------------------------------------------------
    
    if (method == "complete" && tau_method == "b") {
      table_tau$D <- lapply(tau_s, \(.) .$D)|>unlist()
      table_tau$D[1] <- table_tau$pairs[1] - table_tau$ties[1] / 2
    } else {
      table_tau$D <- table_tau$pairs
    }

    # tau -----------------------------------------------------------
    
    table_tau$Tau <- table_tau$S / table_tau$D

    # SD and VAR --------------------------------------------------------------
    
    if (method == "tarlow") {
      table_tau$SD_S <- lapply(tau_s, \(.) .$sdS)|>unlist()
    } else {
      table_tau$SD_S <- c(
        sqrt((nA * nB) * (nA + nB + 1) / 12) * 2,
        kendall_tau(1:nA, 1:nA, tau_method = tau_method)$sdS,
        kendall_tau(1:nB, 1:nB, tau_method = tau_method)$sdS,
        tau_s$AvB_A$sdS,
        tau_s$AvB_B$sdS,
        tau_s$AvB_B_A$sdS
      )
    }
    
    table_tau$VAR_S <- table_tau$SD_S^2

    # Z, p, se ----------------------------------------
    
    table_tau$Z <- lapply(tau_s, \(.) .$z)|>unlist()
    table_tau$p <- lapply(tau_s, \(.) .$p)|>unlist()
    
    table_tau$SE_Tau <- table_tau$Tau / table_tau$Z
    
    # confidence intervals --------------------
    if (!is.na(ci) && !is.null(ci)) {
      if (ci_method == "s") {
        see <- qnorm((1 - ci) / 2, lower.tail = FALSE)
        S <- table_tau$S
        if (continuity_correction) S <- S - 1
        cis <- list(
          tau_ci_lower = (S - table_tau$SD_S * see) / table_tau$D, 
          tau_ci_upper = (S + table_tau$SD_S * see) / table_tau$D
        )
      } else {
        cis <- .tau_ci(
          table_tau$Tau, table_tau$n, ci = ci, se_method = ci_method
        )
      }
    } else {
      cis <- list(tau_ci_lower = NA, tau_ci_upper = NA)
    }
    
    table_tau$`CI lower` <-  cis$tau_ci_lower
    table_tau$`CI upper` <-  cis$tau_ci_upper

    out$table[[case]] <- table_tau
    out$tau_u[[case]] <- c(
      "A vs. B - Trend A" = 
        table_tau["A vs. B - Trend A", "Tau"]
    )
  }
  
  # Meta analysis ----------------------------------------------------
  
  if (meta_analyses) {
    out$Overall_tau_u <- .meta_tau_u(
      out$table, ci = ci, se_method = meta_weight_method
    )
  } else {
    out$Overall_tau_u <- NA
  }
  
  # return ------------------------------------------------------------------
  
  names(out$table) <- names(data)
  names(out$tau_u) <- names(data)
  
  class(out) <- c("sc_tauu")
  attr(out, opt("phase")) <- pvar
  attr(out, opt("dv")) <- dvar
  out
}


.meta_tau_u <- function(tau_matrix, ci, se_method) {
  
  ci_z <- qnorm((1 - ci) / 2, lower.tail = FALSE)
  
  .meta <- function(model) {
    tau <- sapply(tau_matrix, function(x) x[model, "Tau"])
    n <- sapply(tau_matrix, function(x) x[model, "n"])
    out <- data.frame(Model = model)
  
    res <- .meta_tau(tau, n, ci = ci, se_method = se_method)      

    out$Tau_U <- res$tau
    out$se <- res$se
    out$'CI lower' <- res$lower
    out$'CI upper' <- res$upper
    out$z <- res$z
    out$p <- res$p
    
    out
  }
  
  out <- data.frame(
    Model = character(4), 
    Tau_U = numeric(4),
    se = numeric(4),
    'CI lower' = numeric(4),
    'CI upper' = numeric(4),
    z = numeric(4),
    p = numeric(4),
    check.names = FALSE
  )
  
  models <- c("A vs. B", 
              "A vs. B - Trend A", 
              "A vs. B + Trend B", 
              "A vs. B + Trend B - Trend A"
            )
  
  for(i in 1:length(models)) out[i,] <- .meta(models[i])
  
  out
}

