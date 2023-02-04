#' @rdname export
#' @export
export.sc_desc <- function(object, caption = NA, footnote = NA, filename = NA,
                           kable_styling_options = list(), 
                           kable_options = list(), 
                           flip = FALSE, 
                           ...) {
  
  kable_options <- .join_kabel(kable_options)
  kable_styling_options <- .join_kabel_styling(kable_styling_options)
  
  if (is.na(caption)) caption <- "Descriptive statistics"
  kable_options$caption <- caption
  
  if (is.na(footnote)) {
    footnote <- paste0(
      "n = Number of measurements; ",
      "Missing = Number of missing values; ",
      "M = Mean; ",
      "Median = Median; ",
      "SD = Standard deviation; ",
      "MAD = Median average deviation; ",
      "Min = Minimum; ",
      "Max = Maximum; ",
      "Trend = Slope of dependent variable regressed on measurement-time."
    )
  }
  
  if (flip) {
    object$descriptives[-1:-2] <- round(object$descriptives[-1:-2], kable_options$digits)
    out <- as.data.frame(t(object$descriptives[-1]))
    colnames(out) <- object$descriptives$Case
    rownames(out) <- gsub("mis", "Missing", rownames(out))
    rownames(out) <- gsub("med", "Median", rownames(out))
    rownames(out) <- gsub("min", "Min", rownames(out))
    rownames(out) <- gsub("max", "Max", rownames(out))
    rownames(out) <- gsub("trend", "Trend", rownames(out))
    rownames(out) <- gsub("\\.", " ", rownames(out))
    out <- cbind(Parameter = rownames(out), out)
    rownames(out) <- NULL
    kable_options$align <- c("l", rep("c", 9))
    kable_options$x <- out
    table <- do.call(kable, kable_options)
    kable_styling_options$kable_input <- table
    table <- do.call(kable_styling, kable_styling_options)
  }
  
  
  if (!flip) {
    n_phases <- length(object$design)
    out <- object$descriptives
    colnames(out) <- c("Case", "Design", rep(object$design, 9))
    rownames(out) <- NULL
    kable_options$align <- c("l", rep("c", 9 * n_phases))
    kable_options$x <- out
    table <- do.call(kable, kable_options)
    kable_styling_options$kable_input <- table
    table <- do.call(kable_styling, kable_styling_options)
    table <- add_header_above(
      table,
      c(
        " " = 2, "n" = n_phases,
        "Missing" = n_phases,
        "M" = n_phases,
        "Median" = n_phases,
        "SD" = n_phases,
        "MAD" = n_phases,
        "Min" = n_phases,
        "Max" = n_phases,
        "Trend" = n_phases
      )
    )
  }
  
  if (!is.na(footnote) && footnote != "") 
    table <- footnote(table, general = footnote, threeparttable = TRUE)
  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) cat(table, file = filename)
  table
}
