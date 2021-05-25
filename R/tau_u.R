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
#' @param fisher If TRUE, meta analyses of Tau U is based on Fisher Z transformed correlations.
#' @param continuity_correction If TRUE, a continuity correction is applied for calculating p-values of correlations. This parameter is not yet implemented.
#' @return \item{table}{A data frame containing statistics from the Tau-U
#' family, including: Pairs, positive and negative comparisons, S, and Tau}
#' \item{matrix}{The matrix of comparisons used for calculating the
#' statistics.} \item{tau_u}{Tau-U value.}
#' @details Tau-U is an inconsistently operationalized construct. 
#' @author Juergen Wilbert
#' @family overlap functions
#' @references Parker, R. I., Vannest, K. J., Davis, J. L., & Sauber, S. B.
#' (2011). Combining Nonoverlap and Trend for Single-Case Research: Tau-U.
#' \emph{Behavior Therapy, 42}, 284-299.
#' @examples
#'
#' ## Calculate tau-U for the example from Parker et al. (2011)
#' bob <- scdf(c(A = 2, 3, 5, 3, B = 4, 5, 5, 7, 6), name = "Bob")
#' tau_u(bob)
#'
#' ## Calculate tau-U based on Kendall Tau A
#' tau_u(Grosche2011$Eva, tau_method = "a")
#'
#' ## Request tau-U for all single-cases from the Grosche2011 data set
#' tau_u(Grosche2011)
#' @export

tau_u <- function(data, dvar, pvar, 
                  tau_method = "b", 
                  method = "complete", 
                  phases = c(1, 2), 
                  meta = TRUE,
                  fisher = TRUE, 
                  continuity_correction = FALSE) {
  
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
    "kendall", "k_p", "n", "pairs", "pos", "neg", "ties", "S", "D", "Tau",
    "SD_S", "VAR_S", "SE_Tau", "Z", "p"
  )
  
  # tau-U for each case
  for (i in 1:N) {
    table_tau <- matrix(
      NA, length(row_names), length(col_names), 
      dimnames = list(row_names, col_names)
    )
    table_tau <- as.data.frame(table_tau)
    
    # Extract A and B phase vaues
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
    table_tau$k_p <- c(
      AvBKen$p,
      AvAKen$p,
      BvBKen$p,
      #BvB_AKen$p,
      AvB_AKen$p,
      AvB_BKen$p,
      AvB_B_AKen$p
    )
    table_tau$n <- c(
      AvBKen$N,
      AvAKen$N,
      BvBKen$N,
      #BvB_AKen$N,
      AvB_AKen$N,
      AvB_BKen$N,
      AvB_B_AKen$N
    )
    table_tau$kendall <- c(
      AvBKen$tau,
      AvAKen$tau,
      BvBKen$tau,
      #BvB_AKen$tau,
      AvB_AKen$tau,
      AvB_BKen$tau,
      AvB_B_AKen$tau
    )
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
      #AvA_pair + BvB_pair, # A vs. A - B vs. B
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
      #BvBpos + AvAneg,
      AvBpos + AvAneg,
      AvBpos + BvBpos,
      AvBpos + BvBpos + AvAneg
    )
    
    table_tau$neg <- c(
      AvBneg,
      AvAneg,
      BvBneg,
      #BvBneg + AvApos,
      AvBneg + AvApos,
      AvBneg + BvBneg,
      AvBneg + BvBneg + AvApos
    )
    
    table_tau$ties <- c(
      AvBtie,
      AvAtie,
      BvBtie,
      #BvBtie + AvAtie,
      AvBtie + AvAtie,
      AvBtie + BvBtie,
      AvBtie + BvBtie + AvAtie
    )
    
    # S -----------------------------------------------------------------

    table_tau$S <- table_tau$pos - table_tau$neg
    
    # D -----------------------------------------------------------------------

    if (tau_method == "b") {
      table_tau$D <- c(
        table_tau$pairs[1] - table_tau$ties[1] / 2, #why not AvBKen$D ?
        AvAKen$D,
        BvBKen$D,
        #BvB_AKen$D, #correct? (because see comment three lines above)
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
      #.kendall(1:nAB, c(nA:1, 1:nB), tau_method = tau_method)$sdS,
      AvB_AKen$sdS,
      AvB_BKen$sdS,
      AvB_B_AKen$sdS
    )
    table_tau$VAR_S <- table_tau$SD_S^2
    
    # SE, Z, and p ------------------------------------------------------------
    
    if (tau_method == "b") {
      table_tau$SE_Tau <- table_tau$SD_S / table_tau$D
       #table_tau$Z <- table_tau$S / table_tau$SD_S
    }
    
    if (tau_method == "a") {
      n <- table_tau$n
      table_tau$SE_Tau <- sqrt( (2 * n + 5) / choose(n, 2)) / 3
      table_tau$Z <- table_tau$Tau / table_tau$SE_Tau
      #table_tau$Z2 <- (3 * table_tau$S) / sqrt(n * (n - 1) * (2 * n + 5) / 2)
    }

    table_tau$Z <- table_tau$Tau / table_tau$SE_Tau
    
    table_tau$p <- pnorm(abs(table_tau$Z), lower.tail = FALSE) * 2
    
    out$table[[i]] <- table_tau
    out$matrix[[i]] <- tau_m
    out$tau_u[[i]] <- c(
      "A vs. B + Trend B - Trend A" = table_tau["A vs. B + Trend B - Trend A", "Tau"]
    )
  }
  
  # Overall Tau -------------------------------------------------------------
  
  if (meta) out$Overall_tau_u <- .meta_tau_u(out, fisher = fisher)
  
  # return ------------------------------------------------------------------
  
  out$meta <- meta
  names(out$table) <- names(data)
  names(out$tau_u) <- names(data)
  
  class(out) <- c("sc", "TAU-U")
  out
}



#' @rdname tau_u
#' @export
tauUSC <- function(...) {
  tau_u(...)
}

.meta_tau_u <- function(tau_matrix, fisher = fisher) {
  # Overall Tau -------------------------------------------------------------
  
  .ot <- function(model) {
    ret <- data.frame(Model = model)
    tau <- sapply(tau_matrix$table, function(x) x[model, "Tau"])
    #r <- sin(0.5 * pi * tau)# tau to pearson's r
    n <- sapply(tau_matrix$table, function(x) x[model, "n"])
    se <- sqrt((1 - tau^2)^2/(n - 1))
    weight <- 1 / se^2
    weight[is.infinite(weight)] <- 0
    
    ret$Tau_U <- weighted.mean(tau, weight, na.rm = TRUE)
    ret$se <- sqrt(1 / sum(weight))
    ret$z <- ret$Tau_U / ret$se
    ret$df <- length(tau) - 1
    ret$p <- pnorm(abs(ret$z), lower.tail = FALSE) * 2
    ret
  }
  
  out <- data.frame(
    Model = character(3), 
    Tau_U = numeric(3),
    se = numeric(3),
    z = numeric(3),
    df = numeric(3),
    p = numeric(3)
  )
  
  out[1,] <- .ot("A vs. B") 
  out[2,] <- .ot("A vs. B - Trend A") 
  out[3,] <- .ot("A vs. B + Trend B")
  out[4,] <- .ot("A vs. B + Trend B - Trend A") 
  
  out
}