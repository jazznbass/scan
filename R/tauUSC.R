#' Tau-U for single-case data
#'
#' This function calculates indices of the Tau-U family as proposed by Parker
#' et al. (2011).
#'
#'
#' @inheritParams .inheritParams
#' @param ties.method Defines how to handle ties. \code{"omit"} (default) excludes all
#' ties from the calculation. \code{"positive"} counts all ties as positive
#' comparisons, while \code{"negative"} counts them as negative comparisons.
#' @param method \code{"complete"} (default) or \code{"parker"}. The latter
#' calculates the number of possible pairs as described in Parler et al. (2011)
#' which might lead to tau-U values greater than 1.
#' @return \item{table}{A data frame containing statistics from the Tau-U
#' family, including: Pairs, positive and negative comparisons, S, and Tau}
#' \item{matrix}{The matrix of comparisons used for calculating the
#' statistics.} \item{tau_u}{Tau-U value.}
#' @author Juergen Wilbert
#' @family overlap functions
#' @references Parker, R. I., Vannest, K. J., Davis, J. L., & Sauber, S. B.
#' (2011). Combining Nonoverlap and Trend for Single-Case Research: Tau-U.
#' \emph{Behavior Therapy, 42}, 284-299.
#' @examples
#'
#' ## Calculate tau-U for the example from Parker et al. (2011)
#' bob <- scdf(c(2, 3, 5, 3, 4, 5, 5, 7, 6), B.start = 5)
#' tauUSC(bob)
#'
#' ## Calculate tau-U with ties counted as positive
#' tauUSC(Grosche2011$Eva, ties.method = "positive")
#'
#' ## Request tau-U for all single-cases fom the Grosche2011 data
#' tauUSC(Grosche2011)
#' @export
tauUSC <- function(data, dvar, pvar, ties.method = "omit", method = "complete", phases = c(1, 2)) {

  # set attributes to arguments else set to defaults of scdf
  if (missing(dvar)) dvar <- scdf_attr(data, .opt$dv) else scdf_attr(data, .opt$dv) <- dvar
  if (missing(pvar)) pvar <- scdf_attr(data, .opt$phase) else scdf_attr(data, .opt$phase) <- pvar

  data <- .SCprepareData(data)
  data <- .keepphasesSC(data, phases = phases, pvar = pvar)$data

  N <- length(data)
  out <- list(
    table = list(),
    matrix = list(),
    tau_u = list(),
    N = N,
    method = method
  )
  row_names <- c(
    "A vs. B", "Trend A", "Trend B", "Trend B - Trend A", "A vs. B - Trend A",
    "A vs. B + Trend B", "A vs. B + Trend B - Trend A"
  )
  col_names <- c(
    "pairs", "pos", "neg", "ties", "S", "D", "Tau", "Tau.b", "SD",
    "VAR", "Z", "p"
  )
  for (i in 1:N) {
    table_tau <- matrix(NA, length(row_names), length(col_names), dimnames = list(row_names, col_names))
    table_tau <- as.data.frame(table_tau)

    A <- data[[i]][data[[i]][, pvar] == "A", dvar]
    B <- data[[i]][data[[i]][, pvar] == "B", dvar]
    AB <- c(A, B)
    nA <- length(A)
    nB <- length(B)
    nAB <- nA + nB

    tau_m <- matrix(NA, nrow = nAB, ncol = nAB, dimnames = list(AB, AB))
    tmp <- t(sapply(AB, function(x) x > AB))
    tau_m[tmp] <- "-"
    tmp <- t(sapply(AB, function(x) x < AB))
    tau_m[tmp] <- "+"
    tmp <- t(sapply(AB, function(x) x == AB))
    tau_m[tmp] <- "T"

    diag(tau_m) <- 0
    tau_m[lower.tri(tau_m)] <- ""

    pos.s <- c("+")
    neg.s <- c("-")
    tie.s <- c("T")
    if (ties.method == "positive") pos.s <- c("+", "T")
    if (ties.method == "negative") neg.s <- c("-", "T")

    AvBm <- tau_m[1:nA, (nA + 1):nAB]
    AvBpos <- sum(AvBm %in% pos.s)
    AvBneg <- sum(AvBm %in% neg.s)
    AvBtie <- sum(AvBm %in% tie.s)

    AvAm <- tau_m[1:nA, 1:nA]
    AvApos <- sum(AvAm %in% pos.s)
    AvAneg <- sum(AvAm %in% neg.s)
    AvAtie <- sum(AvAm %in% tie.s)

    BvBm <- tau_m[(nA + 1):nAB, (nA + 1):nAB]
    BvBpos <- sum(BvBm %in% pos.s)
    BvBneg <- sum(BvBm %in% neg.s)
    BvBtie <- sum(BvBm %in% tie.s)

    AvBKen <- .kendall(AB, c(rep(0, nA), rep(1, nB)))
    AvAKen <- .kendall(A, 1:nA)
    BvBKen <- .kendall(B, 1:nB)
    BvB_AKen <- .kendall(AB, c(nA:1, 1:nB))
    AvB_B_AKen <- .kendall(AB, c(nA:1, (nA + 1):nAB))
    AvB_AKen <- .kendall(AB, c(nA:1, rep(nA + 1, nB)))
    AvB_BKen <- .kendall(AB, c(rep(0, nA), (nA + 1):nAB))

    AvB_pair <- nA * nB
    AvA_pair <- (nA * (nA - 1)) / 2
    BvB_pair <- (nB * (nB - 1)) / 2
    ABvAB_pair <- (nAB * (nAB - 1)) / 2

    pairs <- c(
      AvB_pair, # A vs. B
      AvA_pair, # A vs. A
      BvB_pair, # B vs. B
      AvA_pair + BvB_pair, # A vs. A - B vs. B
      # (nAB*(nAB-1))/2,
      AvB_pair + AvA_pair, # A vs. B - A vs. A
      AvB_pair + BvB_pair # A vs. B + B vs. B
    )

    if (method == "parker") table_tau$pairs <- c(pairs, ABvAB_pair) # A vs. B + B vs. B - A vs. A
    if (method == "complete") table_tau$pairs <- c(pairs, AvB_pair + AvA_pair + BvB_pair) # A vs. B + B vs. B - A vs. A)

    table_tau$pos <- c(
      AvBpos,
      AvApos,
      BvBpos,
      BvBpos + AvAneg,
      # AvApos+BvBpos+AvBpos,
      AvBpos + AvAneg,
      AvBpos + BvBpos,
      AvBpos + BvBpos + AvAneg
    )

    table_tau$neg <- c(
      AvBneg,
      AvAneg,
      BvBneg,
      BvBneg + AvApos,
      # AvAneg+BvBneg+AvBneg,
      AvBneg + AvApos,
      AvBneg + BvBneg,
      AvBneg + BvBneg + AvApos
    )

    table_tau$ties <- c(
      AvBtie,
      AvAtie,
      BvBtie,
      BvBtie + AvAtie,
      # AvAtie+BvBtie+AvBtie,
      AvBtie + AvAtie,
      AvBtie + BvBtie,
      AvBtie + BvBtie + AvAtie
    )

    table_tau$S <- table_tau$pos - table_tau$neg
    table_tau$D <- c(
      table_tau$pairs[1] - table_tau$ties[1] / 2,
      AvAKen$D,
      BvBKen$D,
      BvB_AKen$D,
      # NA,
      AvB_AKen$D,
      AvB_BKen$D,
      AvB_B_AKen$D
    )


    table_tau$Tau <- table_tau$S / table_tau$pairs
    table_tau$Tau.b <- table_tau$S / table_tau$D
    table_tau$SD <- c(
      sqrt((nA * nB) * (nA + nB + 1) / 12) * 2,
      sqrt(.kendall(sample(nA), 1:nA)$varS),
      sqrt(.kendall(sample(nB), 1:nB)$varS),
      sqrt(.kendall(sample(nA + nB), c(nA:1, 1:nB))$varS),
      # NA,
      sqrt(AvB_AKen$varS),
      sqrt(AvB_BKen$varS),
      sqrt(AvB_B_AKen$varS)
    )
    table_tau$VAR <- table_tau$SD^2
    table_tau$SE.Tau.b <- table_tau$SD / table_tau$D

    table_tau$Z <- table_tau$S / table_tau$SD
    table_tau$p <- pnorm(abs(table_tau$Z), lower.tail = FALSE) * 2

    out$table[[i]] <- table_tau
    out$matrix[[i]] <- tau_m
    out$tau_u[[i]] <- c("A vs. B + Trend B - Trend A" = table_tau["A vs. B + Trend B - Trend A", "Tau"])
  }
  weight.t <- c()
  weight.v <- c()
  for (i in 1:N) {
    weight.v <- c(weight.v, 1 / out$table[[i]]["A vs. B + Trend B - Trend A", "VAR"])
    weight.t <- c(weight.t, out$table[[i]]["A vs. B + Trend B - Trend A", "Tau"])
  }
  out$Overall_tau_u <- c("A vs. B + Trend B - Trend A" = sum(weight.v * weight.t) / sum(weight.v))

  weight.t <- c()
  weight.v <- c()
  for (i in 1:N) {
    weight.v <- c(weight.v, 1 / out$table[[i]]["A vs. B - Trend A", "VAR"])
    weight.t <- c(weight.t, out$table[[i]]["A vs. B - Trend A", "Tau"])
  }
  out$Overall_tau_u <- c(out$Overall_tau_u, "A vs. B - Trend A" = sum(weight.v * weight.t) / sum(weight.v))
  names(out$table) <- names(data)
  names(out$tau_u) <- names(data)

  class(out) <- c("sc", "TAU-U")

  out
}
