#' Export scan objects to html or latex
#' 
#' This function is in an experimental status.
#' Export creates html files of tables or displayes them directly in the viewer pane of rstudio.
#' When applied in rmarkdown, tables can also be created for pdf/latex output.
#'
#' @param object An scdf
#' @param filename Character string with the filename. If a filename is provided
#' the output will be written into this file.
#' @param kable_options list with arguments passed to the kable function.
#' @param kable_styling_options list with arguments passed to the kable_styling function.
#' @param cols Defines which columns are included when a scdf is exported. It is either a vector 
#' with variable names or the string "main" will select the central variables.
#' @param flip If TRUE, some objects are displayed with rows and columns flipped.
#' @param note If TRUE additional information are printed below the table.
#' @param ... Further Arguments passed to internal functions. 
#' @return  Returns a specif formated html.
#' @export
       
export <- function(object, filename = NULL, 
                   kable_styling_options = list(), kable_options = list(), 
                   cols, flip = FALSE, note = TRUE, ...) {
  
  warning(.opt$function_debugging_warning)  
  
  default_kable_styling <- list(
    bootstrap_options = c("bordered", "condensed"),
    full_width = FALSE,
    position = "center"
  )
  default_kable <- list(
    digits = 2
  )
  
  tmp <- which(!(names(default_kable_styling) %in% names(kable_styling_options)))
  kable_styling_options <- c(kable_styling_options, default_kable_styling[tmp])

  tmp <- which(!(names(default_kable) %in% names(kable_options)))
  kable_options <- c(kable_options, default_kable[tmp])
  
  if (!any(class(object) %in% c("sc","scdf"))) {
    stop("export can only handle objects returned by scan.\n")
  }
  
  x <- class(object)[2]
  
# trend -------------------------------------------------------------------

  if (x == "trend") {
    caption <- c("<i>Trend analysis.</i>")
    out <- object$trend
    if (isTRUE(flip)) out <- t(out)
    
    tmp.rownames  <- rownames(out) 
    rownames(out) <- NULL
    for(tmp in object$formulas)
      tmp.rownames <- gsub(paste0(tmp,"."),"",tmp.rownames)
    out <- cbind(Phase = tmp.rownames,out)
    
    kable_options$x <- out
    table <- do.call(kable, kable_options)
    kable_styling_options$kable_input <- table
    table <- do.call(kable_styling, kable_styling_options)
    
    for(i in 1:length(object$formulas)) {
      table <- group_rows(table, object$formulas[i], 
      1 + (i - 1) * (length(object$design) + 1), 
      i * (length(object$design) + 1))
    }
    tmp <- attributes(table)
    table <- paste0(caption, table)
    attributes(table) <- tmp
    
  }

# describe ----------------------------------------------------------------

  if (x == "describe") {
    caption  <- "Descriptive statistics."
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
 
    n.phases <- length(object$design)
    out <- object$descriptives
    colnames(out) <- rep(object$design, 9)

    kable_options$align <- c("c", rep("c", 9 * n.phases))
    kable_options$x <- out
    table <- do.call(kable, kable_options)
    kable_styling_options$kable_input <- table
    table <- do.call(kable_styling, kable_styling_options)
    table <- add_header_above(table, 
      c(" " = 1, "n" = n.phases, 
        "Missing" = n.phases, 
        "M" = n.phases,
        "Median" = n.phases,
        "SD" = n.phases,
        "MAD" = n.phases,
        "Min" = n.phases,
        "Max" = n.phases,
        "Trend" = n.phases
      )
    )
    if(!note) footnote <- ""
      #table <- footnote(table, general = footnote)
    tmp <- attributes(table)
    table <- paste0(caption, table, footnote)
    attributes(table) <- tmp

    
  }

# overlap -----------------------------------------------------------------

  if (x == "overlap") {
    caption <- c(
      "<i>Overlap indices.",
      .stringPhasesSC(object$design[object$phases.A],object$design[object$phases.B]),
      ".</i>"
    )
    footnote <- c("PND = Percentage Non-Overlapping Data; ",
                  "PEM = Percentage Exceeding the Median; ",
                  "PET = Percentage Exceeding the Trend; ",
                  "NAP = Nonoverlap of all pairs; ",
                  "NAP-R = NAP rescaled; \n",
                  "PAND = Percentage all nonoverlapping data;", 
                  "Tau U = Parker's Tau-U; ",
                  "Base Tau = Baseline corrected Tau; ",
                  "Delta M = Mean difference between phases; ",
                  "Delta Trend = Trend difference between phases; ",
                  "SMD = Standardized Mean Difference."
                  )
    footnote <- paste0(footnote, collapse = "")
    caption  <- paste0(caption, collapse = "")
    n.phases <- 2
    out <- object$overlap
    
    colnames(out)[5] <- "NAP-R"
    colnames(out)[7] <- "Tau-U"
    colnames(out)[9] <- "Delta M"
    colnames(out)[10] <- "Delta Trend"
    
    if (isTRUE(flip)) out <- t(out)
    
    kable_options$x <- out
    table <- do.call(kable, kable_options)
    kable_styling_options$kable_input <- table
    table <- do.call(kable_styling, kable_styling_options)
    #table <- footnote(table, general = footnote)
    tmp <- attributes(table)
    if(!note) footnote <- ""
    table <- paste0(caption, table, footnote)
    attributes(table) <- tmp
  }
  
# plm ---------------------------------------------------------------------

  if (x == "pr") {
    caption <- paste0("Piecewise-regression model for variable '", scdf_attr(object,.opt$dv),"'.")
    
    if (object$ar == 0) out <- summary(object$full.model)$coefficients
    if (object$ar  > 0) out <- summary(object$full.model)$tTable
    
    if (nrow(out) == 1) {
      out <- cbind(out[, 1], t(suppressMessages(confint(object$full))), out[, 2:4])
    } else {
      out <- cbind(out[, 1], suppressMessages(confint(object$full)), out[, 2:4])
    }
    out <- as.data.frame(out)
    if (!is.null(object$r.squares)) 
      out$R2 <- c("", format(round(object$r.squares, 2)))
    
    row.names(out) <- .plm.row.names(row.names(out), object)

    if (!is.null(object$r.squares))
      colnames(out) <- c("B","2.5%","97.5%","SE", "t","p", "Delta R\u00b2")		
    if (is.null(object$r.squares))
      colnames(out) <- c("B","2.5%","97.5%","SE", "t","p")		
    
    if (object$family == "poisson" || object$family == "nbinomial") {
      OR <- exp(out[, 1:3])
      Q  <- (OR - 1) / (OR + 1)
      out <- cbind(out[, -7], round(OR, 3), round(Q, 2))
      colnames(out) <- c(
        "B","2.5%","97.5%","SE", "t","p", 
        "Odds Ratio","2.5%", "97.5%","Yule's Q","2.5%", "97.5%"
      )		
      
      Chi <- object$full$null.deviance - object$full$deviance
      DF  <- object$full$df.null - object$full$df.residual
      test <- sprintf("\u0347\u00b2(%d) = %.2f; <i>p</i> = %0.3f; <i>AIC</i> = %.0f", DF, Chi, 1 - pchisq(Chi, df = DF), object$full$aic)
    }
    
    out <- cbind(Parameter = rownames(out),out, stringsAsFactors = FALSE)
    rownames(out) <- NULL
    if (object$family == "gaussian") {
      out$B               <- sprintf("%.2f", out$B)
      out$"2.5%"          <- sprintf("%.2f", out$"2.5%")
      out$"97.5%"         <- sprintf("%.2f", out$"97.5%")
      out$SE              <- sprintf("%.2f", out$SE)
      out$t               <- sprintf("%.2f", out$t)
      out$p               <- .nice.p((out$p))
      out$"Delta R\u00b2" <- gsub("^0\\.",".", out$"Delta R\u00b2")
      #test <- sprintf("<i>F</i>(%d, %d) = %.2f; <i>p</i> = %0.3f; <i>R</i>\u00b2 = %0.3f; Adjusted <i>R</i>\u00b2 = %0.3f", object$F.test["df1"], object$F.test["df2"],object$F.test["F"], object$F.test["p"], object$F.test["R2"], object$F.test["R2.adj"])
      test <- sprintf("F(%d, %d) = %.2f; p %s; R\u00b2 = %0.3f; Adjusted R\u00b2 = %0.3f", object$F.test["df1"], object$F.test["df2"],object$F.test["F"], .nice.p(object$F.test["p"], TRUE), object$F.test["R2"], object$F.test["R2.adj"])
    }
  
    kable_options$x <- out
    kable_options$align <- c("l", rep("r", ncol(out) - 1))
    table <- do.call(kable, kable_options)
    kable_styling_options$kable_input <- table
    table <- do.call(kable_styling, kable_styling_options)
    table <- add_header_above(table, c(" " = 2, "CI(95%)" = 2, " " = 4))
    tmp <- attributes(table)
    table <- footnote(table, general = test)
    table <- paste0(#"<p>Table.<br>\n",
      "<i>", caption, "</i>",
      table)#test)
    attributes(table) <- tmp

    # if (identical(type, "html2")) {
    #   table <- htmlTable(out,
    #     cgroup = c("","", "CI (95%)","","","",""),
    #     n.cgroup = c(1,1,2,1,1,1),
    #     align = "lc",
    #     align.header = "lc",
    #     caption = caption,
    #     tfoot = paste0("<i>Note:</i> ",test),
    #     rnames = FALSE, ...)
    # }

  } 
  
# hplm --------------------------------------------------------------------
  
  if (x == "hplm") {
    caption <- paste0("Hierarchical Piecewise Linear Regression for variable '", scdf_attr(object,.opt$dv),"'.")
    
    Summary <- summary(object$hplm)
    if (object$model$ICC)
      ICC <- sprintf("<i>ICC</i> = %.3f, <i>L</i> = %.1f, <i>p</i> = %.3f", object$ICC$value, object$ICC$L, object$ICC$p)
    else 
      ICC <- ""
    
    footnote <- c(
      paste0("Estimation method ",object$model$estimation.method),
      paste0("Slope estimation method: ",object$model$interaction.method),
      paste0(object$N," cases")#,
    )
    footnote <- paste0(footnote, collapse = "; ")
   
    out <- as.data.frame(summary(object$hplm)$tTable)
    
    row.names(out) <- .plm.row.names(row.names(out), object)

    colnames(out) <- c("B","SE","df","t","p")
    
    SD <- round(as.numeric(VarCorr(object$hplm)[,"StdDev"]),3)
    md <- data.frame("SD" = SD)
    rownames(md) <- names(VarCorr(object$hplm)[,2])
    
    row.names(md) <- .plm.row.names(row.names(md), object)

    if (object$model$lr.test) {
      if (is.null(object$LR.test[[1]]$L.Ratio)) {
        object$LR.test[[1]]$L.Ratio <- NA
        object$LR.test[[1]]$"p-value" <- NA
        object$LR.test[[1]]$df <- NA
      }
      
      md$L  <- c(round(unlist(lapply(object$LR.test, function(x) x$L.Ratio[2])), 2), NA)
      md$df <- c(unlist(lapply(object$LR.test, function(x) {x$df[2] - x$df[1]})), NA)
      md$p  <- c(round(unlist(lapply(object$LR.test, function(x) x$"p-value"[2])), 3), NA)
    }
  
    out$p <- .nice.p(out$p)
    if (!is.null(md$p)) md$p  <- .nice.p(md$p)
    
    out[, ] <- lapply(out[, ], function(x) 
      if (class(x) == "numeric") as.character(round(x, 2)) else x)
    out <- cbind(Parameter = rownames(out), out, stringsAsFactors = FALSE)
    rownames(out) <- NULL
    md[, ] <- lapply(md, function(x) 
      if (class(x) == "numeric") as.character(round(x, 2)) else x)
    md <- cbind(" " = rownames(md), md, stringsAsFactors = FALSE)
    rownames(md) <- NULL

    nrow_out <- nrow(out)
    tmp.row <- (nrow_out + 1):(nrow_out + nrow(md) + 1 + 3)
    out[tmp.row, ] <- ""
    
    tmp.row <- (nrow_out + 1):(nrow_out + nrow(md) + 1)
    out[tmp.row, 1:ncol(md)] <- rbind(colnames(md), md, stringsAsFactors = FALSE)

    out[nrow_out + nrow(md) + 2, 1:2] <- c("AIC", as.character(round(Summary$AIC, 1)))
    out[nrow_out + nrow(md) + 3, 1:2] <- c("BIC", as.character(round(Summary$BIC, 1)))
    if (!is.null(object$ICC))
      out[nrow_out + nrow(md) + 4, 1:4] <- 
        c(
          "ICC", as.character(round(object$ICC$value, 2)),
          paste0("L = ", round(object$ICC$L, 1)),
          paste0("p ", .nice.p(object$ICC$p))
        )
 
    kable_options$x <- out
    kable_options$align <- c("l", rep("r", ncol(out) - 1))
    table <- do.call(kable, kable_options)
    kable_styling_options$kable_input <- table
    table <- do.call(kable_styling, kable_styling_options)
    
    #table <- group_rows(table, paste0("Fixed effects (",deparse(object$model$fixed),")"),
    #                    1,tmp.nrow)
    table <- group_rows(
      table, "Random effects",
      nrow_out + 1, nrow(out) - 3
    )
    table <- row_spec(table, nrow_out + 1, bold = TRUE, color = "black")
    table <- row_spec(table, nrow_out + nrow(md) + 1, hline_after = TRUE)
    #table <- row_spec(table, nrow(out) - 3, hline_after = TRUE)
    table <- row_spec(table, nrow_out, hline_after = TRUE)
    table <- footnote(table, general = footnote)
    
    tmp <- attributes(table)
    table <- paste0(caption, table)
    attributes(table) <- tmp
  }
  
# scdf --------------------------------------------------------------------

  if (identical(class(object)[1],"scdf")) {
    
    N <- cases <- length(object)

    if (missing(cols))
      cols <- names(object[[1]])
    if (identical(cols,"main"))
      cols = c(scdf_attr(object, .opt$phase), scdf_attr(object, .opt$dv), scdf_attr(object, .opt$mt))
    
    names(object) <- .case.names(names(object), length(object))
    
    max.row <- max(unlist(lapply(object, nrow)))
    for(i in 1:cases) {
      n.row <- nrow(object[[i]])
      object[[i]][,scdf_attr(object, .opt$phase)] <- as.character(object[[i]][,scdf_attr(object, .opt$phase)])
      if (n.row < max.row) 
        object[[i]][(n.row + 1):max.row, names(object[[i]])] <- ""
    }
    rows <- max.row
    out <- lapply(object[1:cases], function(x) x[1:rows,cols])
    names <- lapply(out, names)
    out <- as.data.frame(out)
    names(out) <- unlist(names[1:cases])
    
    kable_options$x <- out
    kable_options$align <- rep("c", ncol(out))
    table <- do.call(kable, kable_options)
    kable_styling_options$kable_input <- table
    table <- do.call(kable_styling, kable_styling_options)
    
    case.names <- rep(ncol(out)/N, N)
    names(case.names) <- names(object)
    table <- add_header_above(table, case.names)
    footnote <- ""
    if (!is.null(scdf_attr(object,"info"))) footnote <- scdf_attr(object,"info")
    if (!is.null(scdf_attr(object,"author"))) 
      footnote <- paste0(footnote,"\nAuthor: ",scdf_attr(object,"author"))
    if (footnote != "") table <- footnote(table, general = footnote)
  }

# finish ------------------------------------------------------------------

  if (!is.null(filename)) cat(table, file = filename)
  
  table
}

