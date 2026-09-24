#' Tau-U
#'
#' Indices of the Tau-U family per case and, optionally, an overall Tau-U
#' across cases from a meta-analysis. Tau-U is an inconsistently
#' operationalised construct, so `method` decides which of the published
#' variants is computed.
#'
#' @details Tau-U combines the comparison of the phase A with the phase B
#'   measurements with the trend within either phase. Six models are computed
#'   per case and returned as the rows of `table`: `A vs. B`, `Trend A`,
#'   `Trend B`, `A vs. B - Trend A`, `A vs. B + Trend B` and `A vs. B + Trend B
#'   - Trend A`. The fourth of them is the one usually reported as Tau-U and is
#'   the one returned in `tau_u`. Each model is a Kendall's tau of the values
#'   against a rank vector built for that model, so `S` is the number of
#'   concordant minus discordant pairs and `Tau` is `S / D`.
#'
#'   The three methods differ in the denominator `D` and in the p values.
#'   `"complete"` takes `D` from Kendall's tau, tie-corrected for `tau_method =
#'   "b"`. `"parker"` follows Parker et al. (2011b), which counts fewer
#'   possible pairs for the two models that subtract the phase A trend and can
#'   therefore return values outside the \[-1;1\] interval. `"tarlow"` follows
#'   the online calculator and R code of Tarlow (2017). `"parker"` and
#'   `"tarlow"` are only defined for tau-a, so both set `tau_method` to `"a"`;
#'   `"parker"` also switches the continuity correction off and `"tarlow"`
#'   switches it on, whatever was passed.
#'
#'   Confidence intervals for the single cases are obtained by transforming tau
#'   to Fisher's Z, adding the interval and transforming back (Long & Cliff,
#'   1997). Its standard error is `1 / sqrt(n - 3)` for `ci_method = "z"` and
#'   `sqrt(0.437 / (n - 4))` for `"tau"`. `ci_method = "s"` instead builds the
#'   interval on the scale of `S` and divides by `D`; it is not recommended.
#'   `SE_Tau` in the table is derived as `Tau / Z`.
#'
#'   The meta-analysis weights each case by the inverse variance of its
#'   Fisher-Z transformed tau and reports the back-transformed fixed-effect
#'   estimate. It covers four of the six models; the two pure trend models are
#'   left out. A model in which any case has a missing tau gives `NA`.
#'
#'   Missing values are dropped beforehand. A case with fewer than two observed
#'   measurements in one of the phases is set to `NA` throughout with a
#'   warning, which also makes every meta-analytic model `NA`.
#' @inheritParams .inheritParams
#' @param method Character with values `"complete"`, `"parker"` or `"tarlow"`
#'   indicating which operationalisation of Tau-U is computed.
#' @param meta_analyses If TRUE, a meta analysis across cases is conducted.
#' @param meta_weight_method Character with values `"z"` or `"tau"` indicating
#'   how the standard error used for weighting the cases is calculated.
#' @param ci Confidence interval level. If NULL or NA, no confidence intervals
#'   are calculated.
#' @param ci_method Character with values `"z"`, `"tau"` or `"s"` indicating how
#'   the standard error of tau is calculated.
#' @param tau_method Character with values `"a"` or `"b"` indicating whether
#'   Kendall's Tau A or Tau B is applied. Ignored for methods `"tarlow"` and
#'   `"parker"`.
#' @param continuity_correction If TRUE, S is reduced by one before calculating
#'   Z, which lowers the p values. Ignored for methods `"tarlow"` and
#'   `"parker"`.
#' @return An object of class `sc_tauu` with the elements:
#'  |  |  |
#'  | --- | --- |
#'  | `tau_u` | Tau of the model `A vs. B - Trend A` per case. |
#'  | `table` | One data frame per case with the six models in the rows and `pairs`, `pos`, `neg`, `ties`, `S`, `D`, `Tau`, `CI lower`, `CI upper`, `SD_S`, `VAR_S`, `SE_Tau`, `Z`, `p` and `n` in the columns. |
#'  | `Overall_tau_u` | Meta-analytic Tau-U across all cases for four of the six models, with standard error, confidence interval, z and p. |
#'  | `n_cases` | Number of cases. |
#' @author Juergen Wilbert
#' @family overlap functions
#' @references Brossart, D. F., Laird, V. C., & Armstrong, T. W. (2018).
#'   Interpreting Kendall's Tau and Tau-U for single-case experimental designs.
#'   \emph{Cogent Psychology, 5(1)}, 1-26.
#'   https://doi.org/10.1080/23311908.2018.1518687.
#'
#'   Long, J. D., & Cliff, N. (1997). Confidence intervals for Kendall's tau.
#'   \emph{British Journal of Mathematical and Statistical Psychology}, 50(1),
#'   31-41. https://doi.org/10.1111/j.2044-8317.1997.tb01100.x
#'
#'   Parker, R. I., Vannest, K. J., & Davis, J. L. (2011a). Effect Size in
#'   Single-Case Research: A Review of Nine Nonoverlap Techniques.
#'   \emph{Behavior Modification}, 35(4), 303-322. https://doi.org/10/dsdfs4
#'
#'   Parker, R. I., Vannest, K. J., Davis, J. L., & Sauber, S. B. (2011b).
#'   Combining Nonoverlap and Trend for Single-Case Research: Tau-U.
#'   \emph{Behavior Therapy, 42}(2), 284-299.
#'   https://doi.org/10.1016/j.beth.2010.08.006
#'
#'   Tarlow, K. R. (2017, March). Tau-U for single-case research (R code).
#'   Retrieved from http://ktarlow.com/stats/
#' @examples
#' tau_u(exampleAB)
#'
#' # all six models of every case
#' print(tau_u(exampleAB), complete = TRUE)
#'
#' # the operationalisation of Parker et al. (2011b)
#' tau_u(exampleAB, method = "parker")
#' @order 1
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
    by_call(tau_method),
    by_call(ci_method),
    by_call(method),
    by_call(meta_weight_method),
    is_true(
      is.null(ci) || is.na(ci) || (ci > 0 && ci < 1),
      "Argument ci must be NULL, NA, or a value between 0 and 1."
    )
  )
  
  # NULL and NA both mean: no confidence intervals
  if (is.null(ci) || is.na(ci)) ci <- NA_real_
  
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
    "SD_S", "VAR_S", "SE_Tau", "Z", "p", "n"
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
    
    if (nA < 2L || nB < 2L) {
      warn(
        "Case ", case, ": less than two observed values in a phase. ",
        "Tau-U is set to NA."
      )
      out$table[[case]] <- table_tau
      out$tau_u[[case]] <- c("A vs. B - Trend A" = NA_real_)
      next
    }
    
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
    if (!is.null(ci) && !is.na(ci)) {
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
  attributes(out)[opts("phase", "dv")] <- list(pvar, dvar)
  out
}


.meta_tau_u <- function(tau_matrix, ci, se_method) {
  
  ci_z <- qnorm((1 - ci) / 2, lower.tail = FALSE)
  
  .meta <- function(model) {
    tau <- sapply(tau_matrix, function(x) x[model, "Tau"])
    n <- sapply(tau_matrix, function(x) x[model, "n"])
    
    if (anyNA(tau) || anyNA(n)) {
      return(data.frame(
        Model = model,
        Tau_U = NA_real_,
        se = NA_real_,
        'CI lower' = NA_real_,
        'CI upper' = NA_real_,
        z = NA_real_,
        p = NA_real_,
        check.names = FALSE
      ))
    }
    
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

