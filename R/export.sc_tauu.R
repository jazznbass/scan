#' @rdname export
#' @param case For tau_u(): Either "meta" for exporting the results of the meta analyses or "all" exporting tau-u for each single-case.
#' @param select Character vector with name of variables to be included. When the vector is named, variables are renamed appropriately.
#' @export
export.sc_tauu <- function(object, 
                           caption = NA, 
                           footnote = NA, 
                           filename = NA,
                           select = "auto", 
                           kable_styling_options = list(), 
                           kable_options = list(),
                           case = "meta",
                           ...) {
  
  kable_options <- .join_kabel(kable_options)
  kable_styling_options <- .join_kabel_styling(kable_styling_options)
  
  if (is.na(caption)) {
    #A <- object$design[object$phases.A]
    #B <- object$design[object$phases.B]
    if (case == "meta") 
      caption <- c("Overall Tau-U") 
    else 
      caption <- "Tau-U analyses"
    #, .phases_string(A, B))
  }
  kable_options$caption <- caption
  
  if (is.na(footnote)) {
    footnote <- paste(
      "Method is '", object$method, 
      "'. Analyses based on Kendall's Tau ", object$tau_method, ". ",
      object$ci * 100, "% CIs for tau are reported. ",
      collapse = ""
    )
  }
  
  if (case == "meta") out <- object$Overall_tau_u
  
  if (case == "all") {
    tables <- object$table
    names_models <- c(" ", row.names(tables[[1]]))
    n_cases <- length(tables)
    out <- rbind(tables[[1]])
    out <- rbind(NA, out)
    
    for(i in 2:n_cases) out <- rbind(out, NA, tables[[i]])
  
    row.names(out) <- NULL
    Model <- rep(names_models, length = nrow(out)) 
    Case <- lapply(
      names(tables), 
      function(x) c(x, rep(" ", length(names_models) - 1))
    )
    Case <- unlist(Case)
    out <- cbind(Case, Model, out)
    
  }  
  
  out$p <- .nice_p(out$p)
  
  if (identical(select, "auto")) {
    if (case == "all") 
      select <- c("Case", "Tau", "CI lower", "CI upper", "Z", "p")
    if (case == "meta") 
      select <- c(
        "Model", "Tau U" = "Tau_U", "se", "CI lower", "CI upper", "z", "p"
      )
  }
  out <- .select(out, select)
  
  opts <- options(knitr.kable.NA = "")
  
  kable_options$x <- out
  kable_options$align <- c("l", rep("r", ncol(out) - 1))
  table <- do.call(kable, kable_options)
  kable_styling_options$kable_input <- table
  table <- do.call(kable_styling, kable_styling_options)
  if (!is.na(footnote) && footnote != "") 
    table <- footnote(table, general = footnote, threeparttable = TRUE)
  
  options(opts)
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) cat(table, file = filename)
  table
}
