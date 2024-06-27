#' @describeIn tau_u Export results as html table
#' @order 3
#' @inheritParams export
#' @param meta If TRUE, the results of the meta analysis will be exported. If
#'   FALSE, each single-case is exported.
#' @param select Character vector with name of variables to be included. When
#'   the vector is named, variables are renamed appropriately.
#' @export
export.sc_tauu <- function(object, 
                           caption = NA, 
                           footnote = NA, 
                           filename = NA,
                           select = "auto", 
                           kable_styling_options = list(), 
                           kable_options = list(),
                           meta = FALSE,
                           round = 3,
                           decimals = 3,
                           ...) {
  
  kable_options <- .join_kabel(kable_options)
  kable_styling_options <- .join_kabel_styling(kable_styling_options)
  
  if (is.na(caption)) {
    #A <- object$design[object$phases.A]
    #B <- object$design[object$phases.B]
    if (meta) 
      caption <- c("Overall Tau-U") 
    else 
      caption <- "Tau-U analyses"
    #, .phases_string(A, B))
  }

  if (is.na(footnote)) {
    footnote <- paste(
      "Method is '", object$method, 
      "'. Analyses based on Kendall's Tau ", object$tau_method, ". ",
      object$ci * 100, "% CIs for tau are reported",
      collapse = ""
    )
  }
  
  if (meta) {
    out <- object$Overall_tau_u
    row_group <- NULL
  }
  
  if (!meta && getOption("scan.export.engine") == "kable") {
    tables <- object$table
    names_models <- c(" ", row.names(tables[[1]]))
    
    n_cases <- length(tables)
    out <- rbind(tables[[1]])
    out <- rbind(NA, out)
    
    for(i in 2:n_cases) out <- rbind(out, NA, tables[[i]])
  
    row.names(out) <- NULL
    Model <- rep(names_models, length = nrow(out)) 
    Case <- lapply(names(tables), function(x) c(x, rep(" ", 6))) |> unlist()
    out <- cbind(Case, Model, out)
    kable_options$align <- c("l", "l", rep("c", ncol(out) - 2))
    
    if (identical(select, "auto")) {
        select <- c("Case", "Model", "Tau", "CI lower", "CI upper", "Z", "p")
    }
    
  }  
  
  if (identical(select, "auto") && meta) {
      select <- c(
        "Model", "Tau U" = "Tau_U", "se", "CI lower", "CI upper", "z", "p"
      )
  }
  
  if (!meta && getOption("scan.export.engine") == "gt") {
    tables <- object$table
    out <- do.call(rbind, tables)
    out <- cbind(Model = rep(rownames(tables[[1]]), length(tables)), out)
    rownames(out) <- NULL
    
    if (identical(select, "auto")) {
        select <- c("Model", "Tau", "CI lower", "CI upper", "Z", "p")
    }
    
    row_group <- vector("list", length(tables))
    names(row_group) <- names(tables)
    
    for (i in 1:length(tables)) {
      .start <- 1 + (i - 1) * nrow(tables[[1]])
      row_group[[i]] <- .start : (.start + nrow(tables[[1]]) - 1)
    }
  }
  
  out$p <- .nice_p(out$p)
  out <- .select(out, select)
  opts <- options(knitr.kable.NA = "")
  

  table <- .create_table(
    out, 
    kable_options, 
    kable_styling_options, 
    caption = caption,
    footnote = footnote,
    row_group = row_group,
    decimals = decimals,
    ...
  )
  
  #if (!meta) {
  #  table <- add_indent(table, (1:nrow(out))[-(1:(nrow(out)/7) * 7 - 6)])
  #}
  
  
  options(opts)
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) .save_export(table, filename)
  table
}
