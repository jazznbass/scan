#' @describeIn corrected_tau Print results
#' @order 2
#' @param x An object returned by [corrected_tau()]
#' @param nice If set TRUE (default) output values are rounded and optimized for
#'  publication tables.
#' @inheritParams print.sc
#' @inheritParams .inheritParams
#' @export
print.sc_bctau <- function(x, nice = TRUE, digits = "auto", ...) {
  
  results <- .output_bctau(x)
  
  cat("Baseline corrected tau\n\n")
  cat(results$notes, sep = "\n")
  cat("\n\n")
  
  tables <- results$tables
  
  for (i in seq_along(tables)) {
    if (digits == "auto") {
      tables[[i]]$p   <- round(tables[[i]]$p, 3)
      tables[[i]]$z   <- sprintf("%.2f", tables[[i]]$z)
      tables[[i]]$tau <- sprintf("%.2f", tables[[i]]$tau)
    } else {
      tables[[i]]$p   <- round(tables[[i]]$p, digits)
      tables[[i]]$z   <- round(tables[[i]]$z, digits)
      tables[[i]]$tau <- round(tables[[i]]$tau, digits)
    }
    
    if (nice) tables[[i]]$p <- .nice_p(tables[[i]]$p)
    
    rownames(tables[[i]]) <- tables[[i]]$Model
    cat(names(tables)[i], ":\n")
    print(tables[[i]][, -1], ...)
    cat("\n")
    
    if (results$correction[[i]]) {
      cat("Baseline correction should be applied.\n\n")
    } else {
      cat("Baseline correction should not be applied.\n\n")
    }
  }
  
  cat("\n")
  
}

#' @describeIn corrected_tau Export results as html
#' @order 3
#' @inheritParams export
#' @inheritParams .inheritParams
#' @export
export.sc_bctau <- function(object, 
                              caption = NA, 
                              footnote = NA, 
                              filename = NA,
                              nice = TRUE, 
                              round = 2,
                              ...) {
  
  if (is.na(caption)) {
    caption <- paste0(
      "Baseline corrected tau for variable '", 
      attr(object, opt("dv")),  "'"
    )
  }
  
  results <- .output_bctau(object)
  
  footnote <- .footnote(footnote, results$notes)
  
  out <- round_numeric(results$stacked, round)
  row_group <- results$row_group
  if (nice) out$p <- .nice_p(out$p)
  
  table <- .create_table(
    out,
    caption = caption,
    footnote = footnote,
    row_group = row_group,
    ...
  )
  
  if (!is.na(filename)) .save_export(table, filename)
  
  table
  
}

# Values of a corrected_tau object, extracted once for the print and the export
# method.
.output_bctau <- function(x) {
  
  out <- list()
  
  out$tables     <- x$corrected_tau
  out$correction <- x$correction
  
  out$notes <- c(
    "Method: Theil-Sen regression",
    paste("Kendall's tau", x$tau_method, "applied"),
    if (x$continuity) {
      "Continuity correction applied"
    } else {
      "Continuity correction not applied"
    }
  )
  
  # the tables of the cases stacked into one, with the recommendation in a
  # column, and the rows each case occupies
  tables <- mapply(
    function(table, apply) {
      table$"Correction recommended?" <- c(
        ifelse(apply, "Yes", "No"), rep("", nrow(table) - 1)
      )
      table
    },
    table = x$corrected_tau, 
    apply = x$correction,
    SIMPLIFY = FALSE
  )
  out$stacked <- do.call(rbind, tables)
  
  rows <- sapply(tables, nrow)
  end <- cumsum(rows)
  start <- end - rows + 1
  out$row_group <- setNames(
    mapply(function(from, to) from:to, start, end, SIMPLIFY = FALSE),
    names(tables)
  )
  
  out
}
