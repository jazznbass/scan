#' @rdname export
#' @export
export.sc_desc <- function(object, 
                           caption = NA, 
                           footnote = NA, 
                           filename = NA,
                           kable_styling_options = list(), 
                           kable_options = list(), 
                           flip = FALSE, 
                           decimals = 2,
                           ...) {
  
  kable_options <- .join_kabel(kable_options)
  kable_styling_options <- .join_kabel_styling(kable_styling_options)
  
  if (is.na(caption)) caption <- "Descriptive statistics"

  if (is.na(footnote)) {
    footnote <- c(
      "n = Number of measurements",
      "Missing = Number of missing values",
      "M = Mean",
      "Median = Median",
      "SD = Standard deviation",
      "MAD = Median average deviation",
      "Min = Minimum",
      "Max = Maximum",
      "Trend = Slope of dependent variable regressed on measurement-time"
    )
  }
  
  object$descriptives[-1:-2] <- round(
    object$descriptives[-1:-2], kable_options$digits
  )
  
  if (flip) {
    out <- as.data.frame(t(object$descriptives[-1]))
    colnames(out) <- object$descriptives$Case
    rownames(out) <- gsub("mis", "Missing", rownames(out))
    rownames(out) <- gsub("med", "Median", rownames(out))
    rownames(out) <- gsub("min", "Min", rownames(out))
    rownames(out) <- gsub("max", "Max", rownames(out))
    rownames(out) <- gsub("trend", "Trend", rownames(out))
    rownames(out) <- gsub("\\.", " ", rownames(out))
    out <- cbind(Parameter = rownames(out), out)

    table <- .create_table(
      out, 
      kable_options, 
      kable_styling_options, 
      caption = caption,
      footnote = footnote
    )
  }
  
  
  if (!flip) {
    n_phases <- length(object$design)
    out <- object$descriptives
    colnames(out) <- c("Case", "Design", rep(object$design, 9))
    spannerpos <- 3 + (0:9 * n_phases)
    
    table <- .create_table(
      out, 
      kable_options, 
      kable_styling_options, 
      caption = caption,
      footnote = footnote,
      spanner = list(
        "n" = spannerpos[1]:(spannerpos[1] + n_phases - 1),
        "Missing" = spannerpos[2]:(spannerpos[2] + n_phases - 1),
        "M" = spannerpos[3]:(spannerpos[3] + n_phases - 1),
        "Median" = spannerpos[4]:(spannerpos[4] + n_phases - 1),
        "SD" = spannerpos[5]:(spannerpos[5] + n_phases - 1),
        "MAD" = spannerpos[6]:(spannerpos[6] + n_phases - 1),
        "Min" = spannerpos[7]:(spannerpos[7] + n_phases - 1),
        "Max" = spannerpos[8]:(spannerpos[8] + n_phases - 1),
        "Trend" = spannerpos[9]:(spannerpos[9] + n_phases - 1)
      ),
      decimals = decimals,
      ...
    )
    
    if (getOption("scan.export.engine") == "kable") {
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
  }
  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) .save_export(table, filename)
  
  table
}
