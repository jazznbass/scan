#' @rdname export
#' @export
export.sc_power <- function(object, caption = NA, footnote = NA, filename = NA,
                            round = 3,
                            ...) {
  
  if (is.na(caption)) {
    caption <- c("Test power in percent")
  }
  
  if (is.na(footnote)) {
  }

  out <- object
  class(out) <- "data.frame"
 
  out <- round_numeric(out, round)
  
  table <- .create_table(
    out, 
    caption = caption,
    footnote = footnote,
    ...
  )
  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) .save_export(table, filename)
  table
}
