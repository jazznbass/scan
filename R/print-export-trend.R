#' @describeIn trend Print results
#' @order 2
#' @inheritParams print.sc
#' @export
#' 
print.sc_trend <- function(x, digits = 3, ...) {
  x$trend <- round(x$trend, digits)
  cat("Trend for each phase\n\n")
  print(x$trend)
  cat("\n")
  cat("Note. Measurement-times start at", x$first_mt, "for each phase\n")
  .note_vars(x)
}

#' @describeIn trend Export results as html table (see [export()])
#' @order 3
#' @inheritParams export
#' @inheritParams .inheritParams
#' @export
export.sc_trend <- function(object, 
                            caption = NA, 
                            footnote = NA, 
                            filename = NA,
                            round = 3,
                            decimals = NULL,
                            ...) {
  
  if (is.na(caption)) caption <- c("Trend analysis")
  footnote <- .footnote(footnote, paste(
      "Measurement-times start at", object$first_mt, " for each phase")
  )
  
  out <- object$trend
  #if (isTRUE(flip)) out <- t(out)
  
  tmp.rownames <- rownames(out)
  rownames(out) <- NULL
  
  for (tmp in names(object$formulas)) {
    hit <- startsWith(tmp.rownames, paste0(tmp, "."))
    tmp.rownames[hit] <- substring(tmp.rownames[hit], nchar(tmp) + 2L)
  }
  out <- cbind(Phase = tmp.rownames, out)
  
  out <- round_numeric(out, round)
  
  row_group <- vector("list", length(object$formulas))
  names(row_group) <- paste0(names(object$formulas), " (", object$formulas,")")
  
  for (i in 1:length(object$formulas)) {
    .start <- 1 + (i - 1) * (length(object$phase_names) + 1)
    row_group[[i]] <- .start : (.start + length(object$phase_names))
  }
  
  table <- .create_table(
    out, 
    caption = caption,
    footnote = footnote,
    row_group = row_group,
    round = round,
    decimals = decimals,
    ...
  )
  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) .save_export(table, filename)
  table
}
