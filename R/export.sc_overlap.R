#' @rdname export
#' @export
export.sc_overlap <- function(object, 
                              caption = NA, 
                              footnote = NULL, 
                              filename = NA,
                              round = 2,
                              decimals = 2,
                              flip = FALSE,
                              ...) {
  
  if (is.na(caption)) caption <- paste0(
    "Overlap indices. ",
    .phases_string(object$phases.A, object$phases.B),
    collapse = ""
  )
  
  if (is.null(footnote)) footnote <- c(
    "PND = Percentage Non-Overlapping Data",
    "PEM = Percentage Exceeding the Median",
    "PET = Percentage Exceeding the Trend",
    "NAP = Nonoverlap of all pairs",
    "NAP-R = NAP rescaled",
    "PAND = Percentage all nonoverlapping data",
    "IRD = Improvement rate difference",
    "Tau U (A + B - trend A) = Parker's Tau-U",
    "Tau U (A + B - trend A + trend B) = Parker's Tau-U",
    "Base Tau = Baseline corrected Tau",
    "Delta M = Mean difference between phases",
    "Delta Trend = Trend difference between phases",
    "SMD = Standardized Mean Difference",
    "Hedges g = Corrected SMD"
  )
  
  out <- object$overlap
  
  cn <- colnames(out)
  colnames(out)[which(cn == "NAP rescaled")] <- "NAP-R"
  colnames(out)[which(cn == "Tau_U(A)")] <- "Tau-U (A + B - trend A)"
  colnames(out)[which(cn == "Tau_U(BA)")] <- "Tau-U (A + B - trend A + trend B)"
  colnames(out)[which(cn == "Base_Tau")] <- "Base Tau"
  colnames(out)[which(cn == "Diff_mean")] <- "Delta M"
  colnames(out)[which(cn == "Diff_trend")] <- "Delta Trend"
  colnames(out)[which(cn == "Hedges_g")] <- "Hedges g"
  
  if (isTRUE(flip)) {
    cases <- out$Case
    out[-2:-1] <- round(out[-2:-1], round)
    names_par <- colnames(out)[-1]
    out <- t(out[-2:-1]) |> as.data.frame()
    out <- cbind(Statistic = rownames(out), out)
    colnames(out) <- c("Statistic", cases)
  }
  
  table <- .create_table(
    out, 
    caption = caption,
    footnote = footnote,
    decimals = decimals,
    ...
  )
 
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) .save_export(table, filename)
  
  table
}
