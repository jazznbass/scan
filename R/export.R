#' Export scan objects to html or latex
#'
#' This function is in an experimental status.
#' Export creates html files of tables or displayes them directly in the viewer pane of rstudio.
#' When applied in rmarkdown, tables can also be created for pdf/latex output.
#'
#' @param object An scdf or an object exported from a scan function.
#' @param caption Character string with table caption. If left NA (default) a caption will be created based on the exported object.
#' @param footnote Character string with table footnote. If left NA (default) a footnote will be created based on the exported object. 
#' @param filename Character string with the filename. If a filename is provided
#' the output will be written into this file.
#' @param kable_options list with arguments passed to the kable function.
#' @param kable_styling_options list with arguments passed to the kable_styling function.
#' @param cols Defines which columns are included when a scdf is exported. It is either a vector
#' with variable names or the string "main" will select the central variables.
#' @param flip If TRUE, some objects are exported with rows and columns flipped.
#' @param round value for the digits argument passed to the internally used round function.
#' @param ... Further Arguments passed to internal functions.
#' @return  Returns a specif formated html (or latex).
#' @export

export <- function (object, ...) {
  UseMethod("export")
}

#' @rdname export
#' @export
export.scdf <- function(object, caption = NA, footnote = NA, filename = NA,
                        kable_styling_options = list(), kable_options = list(),
                        cols, ...) {

  kable_options <- .join_kabel(kable_options)
  kable_styling_options <- .join_kabel_styling(kable_styling_options)
  
  if (!is.na(footnote)) {
    if (!is.null(scdf_attr(object, "info"))) 
      footnote <- scdf_attr(object, "info")
    if (!is.null(scdf_attr(object, "author"))) {
      footnote <- paste(footnote, "\nAuthor:", scdf_attr(object, "author"))
    }
  }
  
  N <- cases <- length(object)
  
  if (is.na(caption)) 
    caption <- paste("Single case data frame with", N, "cases")
  kable_options$caption <- caption

  if (missing(cols)) {
    cols <- names(object[[1]])
  }
  if (identical(cols, "main")) {
    cols <- c(
      scdf_attr(object, .opt$phase), 
      scdf_attr(object, .opt$dv), 
      scdf_attr(object, .opt$mt)
    )
  }

  names(object) <- .case_names(names(object), length(object))

  max_row <- max(unlist(lapply(object, nrow)))
  for (i in 1:cases) {
    n_row <- nrow(object[[i]])
    object[[i]][, scdf_attr(object, .opt$phase)] <- as.character(object[[i]][, scdf_attr(object, .opt$phase)])
    if (n_row < max_row) {
      object[[i]][(n_row + 1):max_row, names(object[[i]])] <- ""
    }
  }
  rows <- max_row
  out <- lapply(object[1:cases], function(x) x[1:rows, cols])
  names <- lapply(out, names)
  out <- as.data.frame(out)
  names(out) <- unlist(names[1:cases])

  kable_options$x <- out
  kable_options$align <- rep("c", ncol(out))
  table <- do.call(kable, kable_options)
  kable_styling_options$kable_input <- table
  table <- do.call(kable_styling, kable_styling_options)

  case_names <- rep(ncol(out) / N, N)
  names(case_names) <- names(object)
  table <- add_header_above(table, case_names)
  if (!is.na(footnote) && footnote != "") 
    table <- footnote(table, general = footnote, threeparttable = TRUE)

  # finish ------------------------------------------------------------------

  if (!is.na(filename)) cat(table, file = filename)

  table
}

#' @rdname export
#' @export
export.sc <- function(object, caption = NA, footnote = NA, filename = NA,
                      kable_styling_options = list(), kable_options = list(),
                      cols, flip = FALSE, round = 2, ...) {
 
  table <- FALSE
  
  kable_options <- .join_kabel(kable_options)
  kable_styling_options <- .join_kabel_styling(kable_styling_options)
  
  if (!any(class(object) %in% c("sc", "scdf"))) {
    stop("export can only handle objects returned by scan.\n")
  }
  
  x <- class(object)[2]
  
  # trend -------------------------------------------------------------------
  
  if (x == "trend") {
    
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
  }
  
  # overlap -----------------------------------------------------------------
  
  if (x == "overlap") {
    
    if (is.na(caption)) caption <- c(
      "Overlap indices. ",
      .phases_string(
       object$phases.A, 
       object$phases.B
        #object$design[object$phases.A], 
        #object$design[object$phases.B]
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
    
    n.phases <- 2
    out <- object$overlap
    
    colnames(out)[7] <- "NAP-R"
    colnames(out)[9] <- "Tau-U"
    colnames(out)[10] <- "Base Tau"
    colnames(out)[11] <- "Delta M"
    colnames(out)[12] <- "Delta Trend"
    colnames(out)[14] <- "Hedges g"
    
    if (isTRUE(flip)) {
      cases <- out$Case
      out[-2:-1] <- round(out[-2:-1], kable_options$digits)
      out <- t(out[-1])
      colnames(out) <- cases
    }
    
    kable_options$x <- out
    table <- do.call(kable, kable_options)
    kable_styling_options$kable_input <- table
    table <- do.call(kable_styling, kable_styling_options)
    if (!is.na(footnote) && footnote != "") 
      table <- footnote(table, general = footnote, threeparttable = TRUE)
  }
  
  # plm ---------------------------------------------------------------------
  
  if (x == "pr") {
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
      out$R2 <- c("", format(round(object$r.squares, 2)))
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
      out <- cbind(out[, -7], round(OR, 3), round(Q, 2))
      colnames(out) <- c(
        "B", "2.5%", "97.5%", "SE", "t", "p",
        "Odds Ratio", "2.5%", "97.5%", "Yule's Q", "2.5%", "97.5%"
      )
      
      Chi <- object$full$null.deviance - object$full$deviance
      DF <- object$full$df.null - object$full$df.residual
      F_test <- sprintf("\u0347\u00b2(%d) = %.2f; <i>p</i> = %0.3f; <i>AIC</i> = %.0f", DF, Chi, 1 - pchisq(Chi, df = DF), object$full$aic)
    }
    
    out <- cbind(Parameter = rownames(out), out, stringsAsFactors = FALSE)
    rownames(out) <- NULL
    if (object$family == "gaussian") {
      out$B <- sprintf("%.2f", out$B)
      out$"2.5%" <- sprintf("%.2f", out$"2.5%")
      out$"97.5%" <- sprintf("%.2f", out$"97.5%")
      out$SE <- sprintf("%.2f", out$SE)
      out$t <- sprintf("%.2f", out$t)
      out$p <- .nice_p((out$p))
      out$"Delta R\u00b2" <- gsub("^0\\.", ".", out$"Delta R\u00b2")
      F_test <- sprintf(
        "F(%d, %d) = %.2f; p %s; R\u00b2 = %0.3f; Adjusted R\u00b2 = %0.3f", 
        object$F.test["df1"], object$F.test["df2"], object$F.test["F"], 
        .nice_p(object$F.test["p"], TRUE), object$F.test["R2"], object$F.test["R2.adj"]
      )
    }
    
    if (is.na(footnote)) footnote <- F_test
    
    kable_options$x <- out
    kable_options$align <- c("l", rep("r", ncol(out) - 1))
    table <- do.call(kable, kable_options)
    kable_styling_options$kable_input <- table
    table <- do.call(kable_styling, kable_styling_options)
    table <- add_header_above(table, c(" " = 2, "CI(95%)" = 2, " " = 4))
    if (!is.na(footnote) && footnote != "") 
      table <- footnote(table, general = footnote, threeparttable = TRUE)
    #tmp <- attributes(table)
    #table <- footnote(table, general = test)
    #table <- paste0( # "<p>Table.<br>\n",
    #  "<i>", caption, "</i>",
    #  table
    #) # test)
    #attributes(table) <- tmp
  }
  
  # tau_u ---------------------------------------------------------------------
  
  if (x == "TAU-U") {
    
    if (is.na(caption)) {
      #A <- object$design[object$phases.A]
      #B <- object$design[object$phases.B]
      caption <- c("Tau-U")#, .phases_string(A, B))
    }
    kable_options$caption <- caption
    
    if (is.na(footnote)) {
      footnote <- paste(
        "Method is '", object$method, 
        "'; Analyses based on Kendall's Tau ", object$tau_method, 
        collapse = ""
      )
    }
    
    out <- object$Overall_tau_u
    out$p <- .nice_p(out$p)
    colnames(out) <- c("Model", "Tau", "SE", "z", "p")
  
    kable_options$x <- out
    kable_options$align <- c("l", rep("r", ncol(out) - 1))
    table <- do.call(kable, kable_options)
    kable_styling_options$kable_input <- table
    table <- do.call(kable_styling, kable_styling_options)
    if (!is.na(footnote) && footnote != "") 
      table <- footnote(table, general = footnote, threeparttable = TRUE)
  }
  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) cat(table, file = filename)
  
  if (isFALSE(table)) warning(paste("No export function is available for object of class", x))
  table
}

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
    if (class(x) == "numeric") as.character(round(x, round)) else x
  )
  out <- cbind(Parameter = rownames(out), out, stringsAsFactors = FALSE)
  rownames(out) <- NULL
  md[, ] <- lapply(md, function(x)
    if (class(x) == "numeric") as.character(round(x, round)) else x
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
  
  table
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

