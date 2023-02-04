#' Export scan objects to html or latex
#'
#' This function is in an experimental status. Export creates html files of
#' tables or displayes them directly in the viewer pane of rstudio. When applied
#' in rmarkdown, tables can also be created for pdf/latex output.
#'
#' @param object An scdf or an object exported from a scan function.
#' @param caption Character string with table caption. If left NA (default) a
#'   caption will be created based on the exported object.
#' @param footnote Character string with table footnote. If left NA (default) a
#'   footnote will be created based on the exported object.
#' @param filename String containing the file name. If a filename is given the 
#' output will be written to that file.
#' @param kable_options list with arguments passed to the kable function.
#' @param kable_styling_options list with arguments passed to the kable_styling
#'   function.
#' @param cols Defines which columns are included when exporting a scdf. It is
#'   either a vector of variable names or the string "main" will select the
#'   central variables.
#' @param flip If TRUE, some objects are exported with rows and columns flipped.
#' @param round Integer passed to the digits argument internally used to round
#'   values.
#' @param select A character vector containing the names of the variables to be
#'   included. If the vector is named, the variables will be renamed
#'   accordingly.
#' @param ... Further Arguments passed to internal functions.
#' @return  Returns or displays a specially formatted html (or latex) file.
#' @export

export <- function (object, ...) {
  #warning(.opt$function_experimental_warning)
  UseMethod("export")
}


#' @rdname export
#' @param nice If set TRUE (default) output values are rounded and optimized for
#' publication tables.
#' @export
export.sc_hplm <- function(object, caption = NA, footnote = NA, filename = NA,
                           kable_styling_options = list(), 
                           kable_options = list(), 
                           round = 2,
                           nice = TRUE,
                           ...) {
  
  kable_options <- .join_kabel(kable_options)
  kable_styling_options <- .join_kabel_styling(kable_styling_options)
  
  if (is.na(caption)) {
    caption <- paste0(
      "Hierarchical Piecewise Linear Regression predicting variable '", 
      attr(object, .opt$dv),  "'"
    )
  }
  kable_options$caption <- caption
  
  summary_model <- summary(object$hplm)
  if (object$model$ICC) {
    ICC <- sprintf(
      "<i>ICC</i> = %.3f, <i>L</i> = %.1f, <i>p</i> = %.3f", 
      object$ICC$value, object$ICC$L, object$ICC$p
    )
  } else {
    ICC <- ""
  }
  
  footnote <- c(
    paste0("Estimation method ", object$model$estimation.method),
    paste0("Slope estimation method: ", object$model$interaction.method),
    paste0(" ", object$model$contrast.method),
    paste0(object$N, " cases")
  )
  footnote <- paste0(footnote, collapse = "; ")
  
  out <- as.data.frame(summary(object$hplm)$tTable)
  
  row.names(out) <- .plm.row.names(row.names(out), object)
  
  colnames(out) <- c("B", "SE", "df", "t", "p")
  
  md <- data.frame(
    "SD" = round(as.numeric(VarCorr(object$hplm)[, "StdDev"]), 3)
  )
  rownames(md) <- names(VarCorr(object$hplm)[, 2])

  row.names(md) <- .plm.row.names(row.names(md), object)
  
  if (object$model$lr.test) {
    if (is.null(object$LR.test[[1]]$L.Ratio)) {
      object$LR.test[[1]]$L.Ratio <- NA
      object$LR.test[[1]]$"p-value" <- NA
      object$LR.test[[1]]$df <- NA
    }
    
    md$L <- c(
      round(unlist(lapply(object$LR.test, function(x) x$L.Ratio[2])), 2), 
      NA
    )
    md$df <- c(
      unlist(lapply(object$LR.test, function(x) {x$df[2] - x$df[1]})), 
      NA
    )
    md$p <- c(
      round(unlist(lapply(object$LR.test, function(x) x$"p-value"[2])), 3), 
      NA
    )
  }
  
  if (nice) {
    out$p <- .nice_p(out$p)
    if (!is.null(md$p)) md$p <- .nice_p(md$p)
  }
  
  out[, ] <- lapply(out[, ], function(x)
    if (inherits(x, "numeric")) as.character(round(x, round)) else x
  )
  out <- cbind(Parameter = rownames(out), out, stringsAsFactors = FALSE)
  rownames(out) <- NULL
  md[, ] <- lapply(md, function(x)
    if (inherits(x, "numeric")) as.character(round(x, round)) else x
  )
  md <- cbind(" " = rownames(md), md, stringsAsFactors = FALSE)
  rownames(md) <- NULL
  
  nrow_out <- nrow(out)
  tmp_row <- (nrow_out + 1):(nrow_out + nrow(md) + 1 + 3)
  out[tmp_row, ] <- ""
  
  tmp_row <- (nrow_out + 1):(nrow_out + nrow(md) + 1)
  out[tmp_row, 1:ncol(md)] <- rbind(colnames(md), md, stringsAsFactors = FALSE)
  
  out[nrow_out + nrow(md) + 2, 1:2] <- c("AIC", as.character(round(summary_model$AIC, 1)))
  out[nrow_out + nrow(md) + 3, 1:2] <- c("BIC", as.character(round(summary_model$BIC, 1)))
  if (!is.null(object$ICC)) {
    out[nrow_out + nrow(md) + 4, 1:4] <-
      c(
        "ICC", as.character(round(object$ICC$value, 2)),
        paste0("L = ", round(object$ICC$L, 1)),
        paste0("p ", .nice_p(object$ICC$p))
      )
  }
  
  kable_options$x <- out
  kable_options$align <- c("l", rep("r", ncol(out) - 1))
  table <- do.call(kable, kable_options)
  kable_styling_options$kable_input <- table
  table <- do.call(kable_styling, kable_styling_options)
  
  table <- pack_rows(table, "Fixed effects", 1,nrow_out, indent = FALSE)
  table <- pack_rows(table, "Random effects", nrow_out + 1, nrow(out) - 3, indent = FALSE)
  table <- pack_rows(table, "Model", nrow(out) - 2, nrow(out), indent = FALSE)
  #table <- row_spec(table, nrow_out + 1, bold = TRUE, color = "black")
  table <- row_spec(table, nrow_out + nrow(md) + 1, hline_after = TRUE)
  # table <- row_spec(table, nrow(out) - 3, hline_after = TRUE)
  table <- row_spec(table, nrow_out, hline_after = TRUE)
  
  if (!is.na(footnote) && footnote != "") 
    table <- footnote(table, general = footnote, threeparttable = TRUE)
  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) cat(table, file = filename)
  table

}

#' @rdname export
#' @param nice If set TRUE (default) output values are rounded and optimized for
#' publication tables.
#' @export
export.sc_plm <- function(object, caption = NA, footnote = NA, filename = NA,
                           kable_styling_options = list(), 
                           kable_options = list(), 
                           round = 2,
                           nice = TRUE,
                           ...) {
  
  kable_options <- .join_kabel(kable_options)
  kable_styling_options <- .join_kabel_styling(kable_styling_options)
  
  if (is.na(caption)) {
    caption <- paste0(
      "Piecewise-regression model predicting variable '", 
      attr(object, .opt$dv), "'"
    )
  }
  kable_options$caption <- caption
  
  if (object$ar == 0) out <- summary(object$full.model)$coefficients
  if (object$ar > 0) out <- summary(object$full.model)$tTable
  
  if (nrow(out) == 1) {
    out <- cbind(out[, 1], t(suppressMessages(confint(object$full))), out[, 2:4])
  } else {
    out <- cbind(out[, 1], suppressMessages(confint(object$full)), out[, 2:4])
  }
  out <- as.data.frame(out)
  if (!is.null(object$r.squares)) {
    out$R2 <- c("", format(round(object$r.squares, round)))
  }
  
  row.names(out) <- .plm.row.names(row.names(out), object)
  
  if (!is.null(object$r.squares)) {
    colnames(out) <- c("B", "2.5%", "97.5%", "SE", "t", "p", "Delta R\u00b2")
  }
  if (is.null(object$r.squares)) {
    colnames(out) <- c("B", "2.5%", "97.5%", "SE", "t", "p")
  }
  
  if (object$family == "poisson" || object$family == "nbinomial") {
    OR <- exp(out[, 1:3])
    Q <- (OR - 1) / (OR + 1)
    out <- cbind(out[, -7], round(OR, 3), round(Q, round))
    colnames(out) <- c(
      "B", "2.5%", "97.5%", "SE", "t", "p",
      "Odds Ratio", "2.5%", "97.5%", "Yule's Q", "2.5%", "97.5%"
    )
    
    Chi <- object$full$null.deviance - object$full$deviance
    DF <- object$full$df.null - object$full$df.residual
    F_test <- sprintf(
      "X\u00b2(%d) = %.2f; p %s; AIC = %.0f", 
      DF, 
      Chi, 
      .nice_p(1 - pchisq(Chi, df = DF), TRUE), 
      object$full$aic
    )
  }
  
  out <- cbind(Parameter = rownames(out), out, stringsAsFactors = FALSE)
  rownames(out) <- NULL
  if (object$family == "gaussian") {
    out$B <- sprintf("%.2f", out$B)
    out$"2.5%" <- sprintf("%.2f", out$"2.5%")
    out$"97.5%" <- sprintf("%.2f", out$"97.5%")
    out$SE <- sprintf("%.2f", out$SE)
    out$t <- sprintf("%.2f", out$t)
    if (nice) out$p <- .nice_p((out$p))
    out$"Delta R\u00b2" <- gsub("^0\\.", ".", out$"Delta R\u00b2")
    F_test <- sprintf(
      "F(%d, %d) = %.2f; p %s; R\u00b2 = %0.3f; Adjusted R\u00b2 = %0.3f", 
      object$F.test["df1"], 
      object$F.test["df2"], 
      object$F.test["F"], 
      .nice_p(object$F.test["p"], TRUE), 
      object$F.test["R2"], object$F.test["R2.adj"]
    )
  }
  
  
  
  if (is.na(footnote)) footnote <- F_test
  
  kable_options$x <- out
  kable_options$align <- c("l", rep("r", ncol(out) - 1))
  table <- do.call(kable, kable_options)
  kable_styling_options$kable_input <- table
  table <- do.call(kable_styling, kable_styling_options)
  
  if (object$family == "gaussian") {
    table <- add_header_above(table, c(" " = 2, "CI(95%)" = 2, " " = 4))
  }
  
  if (object$family %in% c("poisson", "nbinomial")) {
    table <- add_header_above(table, 
      c(" " = 2, "CI(95%)" = 2, " " = 4, "CI(95%)" = 2," " = 1, "CI(95%)" = 2 )
    )
  }
  
  if (!is.na(footnote) && footnote != "") 
    table <- footnote(table, general = footnote, threeparttable = TRUE)
  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) cat(table, file = filename)
  table
  
}


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

#' @rdname export
#' @export
export.sc_trend <- function(object, caption = NA, footnote = NA, filename = NA,
                            kable_styling_options = list(), 
                            kable_options = list(), 
                            round = 2,
                            flip = TRUE,
                            ...) {
  
  kable_options <- .join_kabel(kable_options)
  kable_styling_options <- .join_kabel_styling(kable_styling_options)
  
  if (is.na(caption)) caption <- c("Trend analysis")
  kable_options$caption <- caption
  
  out <- object$trend
  if (isTRUE(flip)) out <- t(out)
  
  tmp.rownames <- rownames(out)
  rownames(out) <- NULL
  for (tmp in object$formulas) {
    tmp.rownames <- gsub(paste0(tmp, "."), "", tmp.rownames)
  }
  out <- cbind(Phase = tmp.rownames, out)
  
  kable_options$x <- out
  table <- do.call(kable, kable_options)
  kable_styling_options$kable_input <- table
  table <- do.call(kable_styling, kable_styling_options)
  
  for (i in 1:length(object$formulas)) {
    table <- group_rows(
      table, object$formulas[i],
      1 + (i - 1) * (length(object$design) + 1),
      i * (length(object$design) + 1)
    )
  }
  
  if (!is.na(footnote) && footnote != "") 
    table <- footnote(table, general = footnote, threeparttable = TRUE)
  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) cat(table, file = filename)
  table
}



#' @rdname export
#' @export
export.sc_power <- function(object, caption = NA, footnote = NA, filename = NA,
                           kable_styling_options = list(), 
                           kable_options = list(), 
                           ...) {
  
  kable_options <- .join_kabel(kable_options)
  kable_styling_options <- .join_kabel_styling(kable_styling_options)
  
  if (is.na(caption)) {
    #A <- object$design[object$phases.A]
    #B <- object$design[object$phases.B]
    caption <- c("Test power in percent")#, .phases_string(A, B))
  }
  kable_options$caption <- caption
  
  if (is.na(footnote)) {
    #footnote <- paste(
    #  "Method is '", object$method, 
    #  "'. Analyses based on Kendall's Tau ", object$tau_method, 
    #  ". ", object$ci * 100, "% CIs for tau are reported. ",
    #  object$meta_method, " effect model applied for meta-analyzes.",
    #  collapse = ""
    #)
  }
  
  out <- object
  class(out) <- "data.frame"
  #column_names <- c("Model", "Tau", "SE", "CI lower", "CI upper", "z", "p")
  #colnames(out) <- column_names
  #out$p <- .nice_p(out$p)
  
  
  kable_options$x <- out
  kable_options$align <- c("l", rep("r", ncol(out) - 1))
  table <- do.call(kable, kable_options)
  kable_styling_options$kable_input <- table
  table <- do.call(kable_styling, kable_styling_options)
  if (!is.na(footnote) && footnote != "") 
    table <- footnote(table, general = footnote, threeparttable = TRUE)
  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) cat(table, file = filename)
  table
}


.select <- function(df, select) {

  if (identical(select, "none") || 
      identical(select, "") ||
      identical(select, NA) ||
      is.null(select) ||
      identical(select, FALSE)) return(df)
  
  if (!all(select %in% names(df)) && !is.numeric(select)) {
    warning("`select` arguments has variable names that are not included in " ,
            "the output table: valid names are: ", 
            paste(names(df), collapse = ", "), ".")
  }
  df <- df[, select]
  if (!is.null(names(select))) {
    if (is.numeric(select)) {
      select <-  setNames(names(df)[select], names(select))
    }
    select <- mapply(function(x, y) ifelse(x == "",y,x), names(select), select)
    names(df) <- select
  }
  df
}

.join_kabel <- function(kable_options) {
  
  default_kable <- getOption("scan.export.kable")
  
  tmp <- which(!(names(default_kable) %in% names(kable_options)))
  kable_options <- c(kable_options, default_kable[tmp])
  
  kable_options
} 





.join_kabel_styling <- function(kable_styling_options) {
  
  default_kable_styling <- getOption("scan.export.kable_styling")
  
  tmp <- which(!(names(default_kable_styling) %in% names(kable_styling_options)))
  kable_styling_options <- c(kable_styling_options, default_kable_styling[tmp])
  
  kable_styling_options
} 

