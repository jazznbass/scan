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