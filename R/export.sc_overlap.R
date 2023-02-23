#' @rdname export
#' @export
export.sc_overlap <- function(object, caption = NA, footnote = NA, 
                              filename = NA,
                              kable_styling_options = list(), 
                              kable_options = list(), 
                              round = 2,
                              flip = FALSE,
                              ...) {
  
  kable_options <- .join_kabel(kable_options)
  kable_styling_options <- .join_kabel_styling(kable_styling_options)
  
  if (is.na(caption)) caption <- c(
    "Overlap indices. ",
    .phases_string(
      object$phases.A, 
      object$phases.B
    )
  )
  
  footnote <- c(
    "PND = Percentage Non-Overlapping Data; ",
    "PEM = Percentage Exceeding the Median; ",
    "PET = Percentage Exceeding the Trend; ",
    "NAP = Nonoverlap of all pairs; ",
    "NAP-R = NAP rescaled; ",
    "PAND = Percentage all nonoverlapping data;",
    "Tau U = Parker's Tau-U; ",
    "Base Tau = Baseline corrected Tau; ",
    "Delta M = Mean difference between phases; ",
    "Delta Trend = Trend difference between phases; ",
    "SMD = Standardized Mean Difference; ",
    "Hedges g = Corrected SMD",
    "."
  )
  footnote <- paste0(footnote, collapse = "")
  caption <- paste0(caption, collapse = "")
  
  kable_options$caption <- caption
  
  out <- object$overlap
  
  colnames(out)[7] <- "NAP-R"
  colnames(out)[9] <- "Tau-U"
  colnames(out)[10] <- "Base Tau"
  colnames(out)[11] <- "Delta M"
  colnames(out)[12] <- "Delta Trend"
  colnames(out)[14] <- "Hedges g"
  
  if (isTRUE(flip)) {
    cases <- out$Case
    out[-2:-1] <- round(out[-2:-1], round)
    out <- t(out[-1])
    colnames(out) <- cases
  }
  
  kable_options$x <- out
  table <- do.call(kable, kable_options)
  kable_styling_options$kable_input <- table
  table <- do.call(kable_styling, kable_styling_options)
  if (!is.na(footnote) && footnote != "") 
    table <- footnote(table, general = footnote, threeparttable = TRUE)
  table
}
