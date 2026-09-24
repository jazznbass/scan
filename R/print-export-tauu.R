#' @describeIn tau_u Print results
#' @order 2
#' @inheritParams print.sc
#' @param x Object returned from [tau_u()].
#' @param complete Print all parameters.
#' @param select Character vector with name of variables to be included. When
#'   the vector is named, variables are renamed appropriately.
#' @param nice_p If TRUE, p-values are printed in publication friendly form.
#' @inheritParams .inheritParams
#' @export
#' 
print.sc_tauu <- function(x, 
                          complete = FALSE, 
                          digits = "auto", 
                          select = c(
                            "Tau", "CI lower", "CI upper", "SD_S", "Z", "p"
                          ), 
                          nice_p = TRUE,
                          ...) {
  
  if (digits == "auto") digits <- 2
  
  results <- .output_tauu(x)
  
  cat("Tau-U\n")
  cat("Method:", results$method, "\n")
  cat("Applied Kendall's Tau-", results$tau_method, "\n", sep = "")
 
  if (!is.na(results$ci)) {
    cat(results$ci * 100, "% CIs for tau are reported.\n", sep = "")
    cat("CI method: ", results$ci_method, "\n\n", sep = "")
  } else cat("\n")
  
  out <- results$tables
  
  if (length(out) > 1 && results$meta_analyses) {
    cat("Tau-U meta analyses:\n")
    
    cat("Weight method: ", results$meta_weight_method, "\n", sep = "")
    if (!is.na(results$ci)) 
      cat(results$ci * 100, "% CIs are reported.\n", sep = "")
    cat("\n")
    print(results$meta, row.names = FALSE, digits = digits)
    cat("\n")
  }
  
  if (!complete) {
    select_vars <- select
    select_rows <- match(results$main_models, row.names(results$tables[[1]]))
    
    out <- lapply(results$tables, function(x) {
      x <- round(x[select_rows, select_vars], digits)
      if (nice_p) x$p <- .nice_p(x$p)
      if (!is.null(names(select))) names(x) <- names(select)
      x
    })
    
  } else {
    out <- lapply(out, function(x) {
      x <- round(x, digits)
      if (nice_p) x$p <- .nice_p(x$p) else x$p <- round(x$p, digits)
      x
    })
  }
  
  for(i in seq_along(out)) {
    cat("Case:", names(out)[i], "\n")
    print(out[[i]], ...)
    cat("\n")
  }
  
}

#' @describeIn tau_u Export results as html table
#' @order 3
#' @inheritParams export
#' @param meta If TRUE, the results of the meta analysis will be exported. If
#'   FALSE, each single-case is exported.
#' @param select Character vector with name of variables to be included. When
#'   the vector is named, variables are renamed appropriately.
#' @inheritParams .inheritParams
#' @export
export.sc_tauu <- function(object, 
                           caption = NA, 
                           footnote = NA, 
                           filename = NA,
                           select = "auto",
                           meta = FALSE,
                           round = 3,
                           decimals = 3,
                           ...) {
  
  if (is.na(caption)) {
    if (meta) 
      caption <- c("Overall Tau-U") 
    else 
      caption <- "Tau-U analyses"
  }

  results <- .output_tauu(object)
  
  footnote <- .footnote(footnote,
    paste0(
      "Method is '", results$method, 
      "'. Analyses based on Kendall's Tau ", results$tau_method, ".",
      if (!is.na(results$ci)) 
        paste0(" ", results$ci * 100, "% CIs for tau are reported")
    )
  )
  
  if (meta) {
    out <- results$meta
    row_group <- NULL
    if (identical(select, "auto")) {
      select <- c(
        "Model", "Tau U" = "Tau_U", "se", "CI lower", "CI upper", "z", "p"
      )
    }
  } else {
    out <- results$stacked
    row_group <- results$row_group
    if (identical(select, "auto")) {
      select <- c("Model", "Tau", "CI lower", "CI upper", "Z", "p")
    }
  }
  
  out$p <- .nice_p(out$p)
  out <- .select(out, select)
  
  tmp <- getOption("scan.export.kable")
  tmp$align <- c("l", rep("c", ncol(out) - 1)) 
  opts <- options(scan.export.kable = tmp, knitr.kable.NA = "")
  on.exit(options(opts), add = TRUE)
  
  table <- .create_table(
    out, 
    caption = caption,
    footnote = footnote,
    row_group = row_group,
    decimals = decimals,
    ...
  )
  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) .save_export(table, filename)
  table
}

# Values of a tau_u object, extracted once for the print and the export method.
.output_tauu <- function(x) {
  
  out <- list()
  
  out$method             <- x$method
  out$tau_method         <- x$tau_method
  out$ci                 <- x$ci
  out$ci_method          <- x$ci_method
  out$meta_analyses      <- x$meta_analyses
  out$meta_weight_method <- x$meta_weight_method
  out$tables             <- x$table
  out$meta               <- x$Overall_tau_u
  
  # the models that are shown unless the complete table is asked for
  out$main_models <- c(
    "A vs. B", 
    "A vs. B - Trend A",
    "A vs. B + Trend B", 
    "A vs. B + Trend B - Trend A"
  )
  
  # the tables of the cases stacked into one, with a column naming the model
  # and the rows each case occupies
  n_rows <- nrow(x$table[[1]])
  stacked <- do.call(rbind, x$table)
  stacked <- cbind(
    Model = rep(rownames(x$table[[1]]), length(x$table)), stacked
  )
  rownames(stacked) <- NULL
  out$stacked <- stacked
  
  out$row_group <- setNames(
    lapply(seq_along(x$table), function(i) {
      start <- 1 + (i - 1) * n_rows
      start:(start + n_rows - 1)
    }),
    names(x$table)
  )
  
  out
}
