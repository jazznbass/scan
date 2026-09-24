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
  
  cat("Tau-U\n")
  cat("Method:", x$method, "\n")
  cat("Applied Kendall's Tau-", x$tau_method, "\n", sep = "")
 
  if (!is.na(x$ci)) {
    cat(x$ci * 100, "% CIs for tau are reported.\n", sep = "")
    cat("CI method: ", x$ci_method, "\n\n", sep = "")
  } else cat("\n")
  
  out <- x$table
  
  if (length(out) > 1 && x$meta_analyses) {
    cat("Tau-U meta analyses:\n")
    
    cat("Weight method: ", x$meta_weight_method, "\n", sep = "")
    if (!is.na(x$ci)) cat(x$ci * 100, "% CIs are reported.\n", sep = "")
    cat("\n")
    print(x$Overall_tau_u, row.names = FALSE, digits = digits)
    cat("\n")
  }
  
  if (!complete) {
    select_vars <- select
    select_rows <- match(
      c(
        "A vs. B", 
        "A vs. B - Trend A",
        "A vs. B + Trend B", 
        "A vs. B + Trend B - Trend A"
      ), row.names(x$table[[1]])
    )
    
    out <- lapply(x$table, function(x) {
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

  footnote <- .footnote(footnote,
    paste(
      "Method is '", object$method, 
      "'. Analyses based on Kendall's Tau ", object$tau_method, ". ",
      object$ci * 100, "% CIs for tau are reported",
      collapse = ""
    )
  )
  
  if (meta) {
    out <- object$Overall_tau_u
    row_group <- NULL
  }
  
  if (identical(select, "auto") && meta) {
      select <- c(
        "Model", "Tau U" = "Tau_U", "se", "CI lower", "CI upper", "z", "p"
      )
  }
  
  if (!meta) {
    tables <- object$table
    out <- do.call(rbind, tables)
    out <- cbind(Model = rep(rownames(tables[[1]]), length(tables)), out)
    rownames(out) <- NULL
    
    if (identical(select, "auto")) {
        select <- c("Model", "Tau", "CI lower", "CI upper", "Z", "p")
    }
    
    row_group <- vector("list", length(tables))
    names(row_group) <- names(tables)
    
    for (i in seq_along(tables)) {
      .start <- 1 + (i - 1) * nrow(tables[[1]])
      row_group[[i]] <- .start : (.start + nrow(tables[[1]]) - 1)
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
