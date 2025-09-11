#' @describeIn corrected_tau Print results
#' @order 2
#' @param x An object returned by [corrected_tau()]
#' @param nice If set TRUE (default) output values are rounded and optimized for
#'  publication tables.
#' @inheritParams print.sc
#' @export
print.sc_bctau <- function(x, nice = TRUE, digits = "auto", ...) {
  
  cat("Baseline corrected tau\n\n")
  
  if (x$repeated) {
    cat("Method: Siegel repeated median regression\n")
  } else {
    cat("Method: Theil-Sen regression\n")
  }
  
  cat("Kendall's tau", x$tau_method, "applied.\n")
  if (x$continuity) {
    cat("Continuity correction applied\n")
  } else {
    cat("Continuity correction not applied.\n")
  }
  cat("\n")
  
  for(i in seq_along(x$corrected_tau)) {
    if (digits == "auto") {
      x$corrected_tau[[i]]$p <- round(x$corrected_tau[[i]]$p, 3)
      x$corrected_tau[[i]]$z <- sprintf("%.2f", x$corrected_tau[[i]]$z)
      x$corrected_tau[[i]]$tau <- sprintf("%.2f", x$corrected_tau[[i]]$tau)
    } else {
      x$corrected_tau[[i]]$p <- round(x$corrected_tau[[i]]$p, digits)
      x$corrected_tau[[i]]$z <- round(x$corrected_tau[[i]]$z, digits)
      x$corrected_tau[[i]]$tau <- round(x$corrected_tau[[i]]$tau, digits)
    }
    
    if (nice) {
      x$corrected_tau[[i]]$p <- .nice_p(x$corrected_tau[[i]]$p)
    }
    
    rownames(x$corrected_tau[[i]]) <- x$corrected_tau[[i]]$Model
    cat(names(x$corrected_tau)[i], ":\n")
    print(x$corrected_tau[[i]][,-1], ...)
    cat("\n")
 
    if (x$correction[[i]]) cat("Baseline correction should be applied.\n\n")
    if (!x$correction[[i]]) cat("Baseline correction should not be applied.\n\n")
  }
  

  
  cat("\n")

  
}

#' @describeIn corrected_tau Export results as html
#' @order 3
#' @inheritParams export
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
  
  if (is.na(footnote)) {
    footnote <- if (object$repeated) {
      "Method: Siegel repeated median regression"
    } else {
      "Method: Theil-Sen regression"
    }
    
    footnote <- c(footnote, paste("Kendall's tau", object$tau_method, "applied"))
    str_cc <- if (object$continuity) {
      "Continuity correction applied"
    } else {
      "Continuity correction not applied"
    }
    footnote <- c(footnote, str_cc)
  }
  
  x <- object$corrected_tau
  
  x <- mapply(
    function(x, apply) {
      x$"Correction recommended?" <- c(ifelse(apply, "Yes", "No"), rep("", nrow(x) - 1))
      x
    },
    x = x, 
    apply = object$correction,
    SIMPLIFY = FALSE
  )
  out <- do.call(rbind, x)
 
  rows <- sapply(x, nrow)
  end <- cumsum(sapply(x, nrow)) 
  start <- end - rows + 1
  row_group <- mapply(function(start, end) start:end, start, end, SIMPLIFY = FALSE)
  names(row_group) <- names(x)
  out <- round_numeric(out, round)
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



