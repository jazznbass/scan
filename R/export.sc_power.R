#' @rdname export
#' @export
export.sc_power <- function(object, caption = NA, footnote = NA, filename = NA,
                            kable_styling_options = list(), 
                            kable_options = list(), 
                            decimals = 3,
                            ...) {
  
  kable_options <- .join_kabel(kable_options)
  kable_styling_options <- .join_kabel_styling(kable_styling_options)
  
  if (is.na(caption)) {
    #A <- object$design[object$phases.A]
    #B <- object$design[object$phases.B]
    caption <- c("Test power in percent")#, .phases_string(A, B))
  }
  
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
  
  
  table <- .create_table(
    out, 
    kable_options, 
    kable_styling_options, 
    caption = caption,
    footnote = footnote,
    decimals = decimals,
    ...
  )
  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) .save_export(table, filename)
  table
}
