#' Tau-U for single-case data
#'
#' This function calculates indices of the Tau-U family as proposed by Parker
#' et al. (2011).
#'
#' @aliases tau_u tauUSC
#' @inheritParams .inheritParams
#' @param tau_method Character with values "a" or "b" (default) indicating whether Kendall Tau A or Kendall Tau B is applied.
#' @param method \code{"complete"} (default) or \code{"parker"}. The latter
#' calculates the number of possible pairs as described in Parker et al. (2011)
#' which might lead to tau-U values greater than 1.
#' @param meta_method Character string. If set "random", a random-effect meta-analysis is calculated. If set "fixed", a fixed-effect meta-analysis is calculated.
#' @param continuity_correction If TRUE, a continuity correction is applied for calculating p-values of correlations. This parameter is not yet implemented.
#' @return \item{table}{A data frame containing statistics from the Tau-U
#' family, including: Pairs, positive and negative comparisons, S, and Tau}
#' \item{matrix}{The matrix of comparisons used for calculating the
#' statistics.} \item{tau_u}{Tau-U value.}
#' @details Tau-U is an inconsistently operationalized construct. Parker et al. (2011) describe a method which may result in Tau-U lager than 1. A different implementation of the method (provided at http://www.singlecaseresearch.org/calculators/tau-u) uses tau-b (instead of tau-a as in the original formulation by Parker). Bossart et. al (2018) describe inconsistencies in the results from this implementation as well. Another problems lies in the calculation in overall Tau-U values from several single cases. This function applies a metaanalyzes to gain the overall values. Each tau value is weighted by the inverse of the variance (ie. the tau standard error). Tau values are not converted to Pearson r values. The argument \code{"meta_method"} calculates a random-effect model ("random") or a fixed effect model ("fixed").
#' @author Juergen Wilbert
#' @family overlap functions
#' @references Brossart, D. F., Laird, V. C., & Armstrong, T. W. (2018). Interpreting Kendall’s Tau and Tau-U for single-case experimental designs. \emph{Cogent Psychology, 5(1)}, 1–26. https://doi.org/10.1080/23311908.2018.1518687.
#' 
#' Parker, R. I., Vannest, K. J., Davis, J. L., & Sauber, S. B.
#' (2011). Combining Nonoverlap and Trend for Single-Case Research: Tau-U.
#' \emph{Behavior Therapy, 42}, 284-299.
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
                  tau_method = "b", 
                  method = "complete", 
                  phases = c(1, 2), 
                  meta_method = "random",
                  continuity_correction = FALSE) {
  
  # validity check
  if (!tau_method %in% c("a", "b")) 
    stop("tau_method muste be 'a' or 'b'.")
  if (!method %in% c("complete", "parker")) 
    stop("method muste be 'complete' or 'parker'.")
  if (!meta_method %in% c("random", "fixed")) 
    stop("meta_method muste be 'random' or 'fixed'.")
  
  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- scdf_attr(data, .opt$dv) else scdf_attr(data, .opt$dv) <- dvar
  if (missing(pvar)) pvar <- scdf_attr(data, .opt$phase) else scdf_attr(data, .opt$phase) <- pvar
  
  data <- .SCprepareData(data)
  data <- .keepphasesSC(data, phases = phases, pvar = pvar)$data
  
  # define "out" data structure
  N <- length(data)
  out <- list(
    table = list(),
    matrix = list(),
    tau_u = list(),
    method = method,
    tau_method = tau_method,
    phases = phases,
    n_cases = N,
    continuity_correction = continuity_correction
  )
  row_names <- c(
    "A vs. B", 
    "Trend A", 
    "Trend B", 
    #"Trend B - Trend A", 
    "A vs. B - Trend A",
    "A vs. B + Trend B", 
    "A vs. B + Trend B - Trend A"
  )
  col_names <- c(
    #"kendall", "k_p", "n", 
    "pairs", "pos", "neg", "ties", "S", "D", "Tau",
    "SD_S", "VAR_S", "SE_Tau", "Z", "p"
  )
  
  # tau-U for each case
  for (i in 1:N) {
    table_tau <- matrix(
      NA, length(row_names), length(col_names), 
      dimnames = list(row_names, col_names)
    )
    table_tau <- as.data.frame(table_tau)
    
    # Extract A and B phase values
    .isA <- data[[i]][[pvar]] == "A"
    .isB <- data[[i]][[pvar]] == "B"
    A <- data[[i]][.isA, dvar]
    B <- data[[i]][.isB, dvar]
    
    # drop NA
    A <- A[!is.na(A)]
    B <- B[!is.na(B)]
    
    #
    AB <- c(A, B)
    nA <- length(A)
    nB <- length(B)
    nAB <- nA + nB
    
    # create tau matrix -----------------------------------------------------
    
    pos_sign <- c("+")
    neg_sign <- c("-")
    tie_sign <- c("T")
    
    tau_m <- matrix(NA, nrow = nAB, ncol = nAB, dimnames = list(AB, AB))
    tau_m[t(sapply(AB, function(x) x > AB))] <- neg_sign
    tau_m[t(sapply(AB, function(x) x < AB))] <- pos_sign
    tau_m[t(sapply(AB, function(x) x == AB))] <- tie_sign
    
    diag(tau_m) <- 0
    tau_m[lower.tri(tau_m)] <- ""
    
    # count pos/neg/tie in sub-matrixes ---------------------------------------
    
    AvB_m <- tau_m[1:nA, (nA + 1):nAB]
    AvBpos <- sum(AvB_m %in% pos_sign)
    AvBneg <- sum(AvB_m %in% neg_sign)
    AvBtie <- sum(AvB_m %in% tie_sign)
    
    AvA_m <- tau_m[1:nA, 1:nA]
    AvApos <- sum(AvA_m %in% pos_sign)
    AvAneg <- sum(AvA_m %in% neg_sign)
    AvAtie <- sum(AvA_m %in% tie_sign)
    
    BvB_m <- tau_m[(nA + 1):nAB, (nA + 1):nAB]
    BvBpos <- sum(BvB_m %in% pos_sign)
    BvBneg <- sum(BvB_m %in% neg_sign)
    BvBtie <- sum(BvB_m %in% tie_sign)
    
    # Kendall tau analyses ----------------------------------------------------
    
    AvBKen <- .kendall(AB, c(rep(0, nA), rep(1, nB)), tau_method = tau_method)
    AvAKen <- .kendall(A, 1:nA, tau_method = tau_method)
    BvBKen <- .kendall(B, 1:nB, tau_method = tau_method)
    #BvB_AKen <- .kendall(AB, c(nA:1, 1:nB), tau_method = tau_method)
    AvB_B_AKen <- .kendall(AB, c(nA:1, (nA + 1):nAB), tau_method = tau_method) 
    AvB_AKen <- .kendall(AB, c(nA:1, rep(nA + 1, nB)), tau_method = tau_method)
    AvB_BKen <- .kendall(AB, c(rep(0, nA), (nA + 1):nAB), tau_method = tau_method)
    
    # experimental ------------------------------------------------------------
    # table_tau$k_p <- c(
    #   AvBKen$p,
    #   AvAKen$p,
    #   BvBKen$p,
    #   #BvB_AKen$p,
    #   AvB_AKen$p,
    #   AvB_BKen$p,
    #   AvB_B_AKen$p
    # )
    
    # table_tau$n <- c(
    #   AvBKen$N,
    #   AvAKen$N,
    #   BvBKen$N,
    #   #BvB_AKen$N,
    #   AvB_AKen$N,
    #   AvB_BKen$N,
    #   AvB_B_AKen$N
    # )
    # table_tau$kendall <- c(
    #   AvBKen$tau,
    #   AvAKen$tau,
    #   BvBKen$tau,
    #   #BvB_AKen$tau,
    #   AvB_AKen$tau,
    #   AvB_BKen$tau,
    #   AvB_B_AKen$tau
    # )
    # ENDE experimental ------------------------------------------------------------
    
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

    table_tau$S <- table_tau$pos - table_tau$neg
    
    # D ----------------------------------------------------------------------
    
    if (tau_method == "b") {
      table_tau$D <- c(
        table_tau$pairs[1] - table_tau$ties[1] / 2,
        AvAKen$D,
        BvBKen$D,
        AvB_AKen$D,
        AvB_BKen$D,
        AvB_B_AKen$D
      )
    }

    if (tau_method == "a") {
      table_tau$D <- table_tau$pairs
    }
    
    # tau -----------------------------------------------------------
    
    table_tau$Tau <- table_tau$S / table_tau$D

    # SD and VAR --------------------------------------------------------------
    
    table_tau$SD_S <- c(
      sqrt((nA * nB) * (nA + nB + 1) / 12) * 2,
      .kendall(1:nA, 1:nA, tau_method = tau_method)$sdS,
      .kendall(1:nB, 1:nB, tau_method = tau_method)$sdS,
      AvB_AKen$sdS,
      AvB_BKen$sdS,
      AvB_B_AKen$sdS
    )
    table_tau$VAR_S <- table_tau$SD_S^2
    
    # SE, Z, and p ------------------------------------------------------------
    
    # if (tau_method == "b") {
    #   #table_tau$SE_Tau <- table_tau$SD_S / table_tau$D
    #   #table_tau$Z <- table_tau$Tau / table_tau$SE_Tau
    # }
    # 
    # if (tau_method == "a") {
    #   #n <- table_tau$n
    #   # n <- c(
    #   #   AvBKen$N,
    #   #   AvAKen$N,
    #   #   BvBKen$N,
    #   #   AvB_AKen$N,
    #   #   AvB_BKen$N,
    #   #   AvB_B_AKen$N
    #   # )
    #   #table_tau$SE_Tau <- sqrt( (2 * n + 5) / choose(n, 2)) / 3
    #   #table_tau$Z <- table_tau$S / table_tau$SD_S
    #   #table_tau$SE_Tau <- table_tau$Tau / table_tau$Z
    #   #table_tau$Z2 <- (3 * table_tau$S) / sqrt(n * (n - 1) * (2 * n + 5) / 2)
    # }

    table_tau$Z <- table_tau$S / table_tau$SD_S
    table_tau$SE_Tau <- table_tau$Tau / table_tau$Z
    table_tau$p <- pnorm(abs(table_tau$Z), lower.tail = FALSE) * 2
    
    out$table[[i]] <- table_tau
    out$matrix[[i]] <- tau_m
    out$tau_u[[i]] <- c(
      "A vs. B + Trend B - Trend A" = table_tau["A vs. B + Trend B - Trend A", "Tau"]
    )
  }
  
  # Overall Tau -------------------------------------------------------------
  
  out$Overall_tau_u <- .meta_tau_u(out$table, method = meta_method)
  
  # return ------------------------------------------------------------------
  
  names(out$table) <- names(data)
  names(out$tau_u) <- names(data)
  
  class(out) <- c("sc", "TAU-U")
  attr(out, .opt$phase) <- pvar
  attr(out, .opt$dv) <- dvar
  out
}



#' @rdname tau_u
#' @export
tauUSC <- function(...) {
  tau_u(...)
}

.meta_tau_u <- function(tau_matrix, method = NA) {
  
  .random <- function(tau, se) {
    res <- metagen(tau, se)
    ret <- list()
    ret$Tau_U <- res$TE.random
    ret$se <- res$seTE.random
    ret$z <- res$zval.random
    ret$p <- res$pval.random
    ret
  }
  
  .fixed <- function(tau, se) {
    res <- metagen(tau, se)
    ret <- list()
    ret$Tau_U <- res$TE.fixed
    ret$se <- res$seTE.fixed
    ret$z <- res$zval.fixed
    ret$p <- res$pval.fixed
    ret
  }
  
  .ot <- function(model) {
    tau <- sapply(tau_matrix, function(x) x[model, "Tau"])
    se <- sapply(tau_matrix, function(x) x[model, "SE_Tau"])
    
    if (method == "random") return(data.frame(Model = model, .random(tau, se)))
    if (method == "fixed") return(data.frame(Model = model, .fixed(tau, se)))
  }
  
  out <- data.frame(
    Model = character(4), 
    Tau_U = numeric(4),
    se = numeric(4),
    z = numeric(4),
    p = numeric(4)
  )
  
  out[1,] <- .ot("A vs. B") 
  out[2,] <- .ot("A vs. B - Trend A") 
  out[3,] <- .ot("A vs. B + Trend B")
  out[4,] <- .ot("A vs. B + Trend B - Trend A") 
  
  out
}