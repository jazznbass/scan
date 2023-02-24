#' @rdname export
#' @export
export.sc_pem <- function(object, 
                            caption = NA, 
                            footnote = NA, 
                            filename = NA,
                            kable_styling_options = list(), 
                            kable_options = list(), 
                            round = 2,
                            ...) {
  
  kable_options <- .join_kabel(kable_options)
  kable_styling_options <- .join_kabel_styling(kable_styling_options)
  
  if (is.na(caption)) caption <- c("Percent Exceeding the Median")
  kable_options$caption <- caption
  
  
  out <- cbind(PEM = object$PEM, object$test)
    
  kable_options$x <- out
  table <- do.call(kable, kable_options)
  kable_styling_options$kable_input <- table
  table <- do.call(kable_styling, kable_styling_options)
  
  if (!is.na(footnote) && footnote != "") 
    table <- footnote(table, general = footnote, threeparttable = TRUE)
  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) cat(table, file = filename)
  table
}