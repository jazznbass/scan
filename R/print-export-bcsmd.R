#' @describeIn between_smd Print results
#' @inheritParams print.sc
#' @order 2
#' @param x An object returned by `baseline_smd()`.
#' @export
print.sc_bcsmd <- function(x, digits = 2, ...) {
  cat("Between-Case Standardized Mean Difference\n")

  cat("\nMethod:", x$method)
  
  for (i in 1:length(x$models)) {
    x$models[[i]] <- round_numeric(x$models[[i]], digits)
    names(x$models[[i]])[4:5] <- str_ci(x$ci)
    cat("\n")
    cat(names(x$models)[i])
    cat("\n\n")
    print(x$models[[i]], row.names = FALSE)
  }
 
}

#' @describeIn between_smd export results
#' @inheritParams export
#' @order 3
#' @param x An object returned by `baseline_smd()`.
#' @export
export.sc_bcsmd <- function(object, 
                            caption = NA, 
                            footnote = NA, 
                            filename = NA,
                            digits = 2, 
                            round = 2,
                            ...) {
  
  if (is.na(caption)) caption <- c("Between-Case Standardized Mean Difference")
  
  if (is.na(footnote)) {
    footnote <- c(
      if (object$method == "MCMCglmm") "CI = credible interval" else "CI = confidence interval",
      "LL = lower limit",
                  "UL = upper limit",
                  paste0("Method: ", object$method))
  }

  for (i in 1:length(object$models)) {
    object$models[[i]] <- round_numeric(object$models[[i]], digits)
    names(object$models[[i]])[4:5] <- c("LL", "UL")
  }
  
  out <- do.call(rbind, object$models)
  
  if (getOption("scan.export.engine") == "gt") {
    spanner <- list("CI" = 4:5)
    names(spanner) <- gsub(
      "CI", paste0("CI(", object$ci * 100, "%)"), x = names(spanner)
    )
  }
  
  if (length(object$models) == 2) {
    rows <- nrow(object$models[[1]])
    row_group <- list(
      "Base model" = 1:rows,
      "Full model" = (rows + 1) : (rows * 2)
    )
    names(row_group) <- names(object$models)
  } else {
    row_group <- NULL
  }
  
  table <- .create_table(
    out, 
    row_group = row_group,
    spanner = spanner,
    caption = caption,
    footnote = footnote,
    digits = digits,
    ...
  )
  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) .save_export(table, filename)
  table
}
