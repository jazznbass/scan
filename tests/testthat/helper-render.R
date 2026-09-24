# The text of an exported table, whatever engine built it. Html entities and
# the typographic quotes that the markdown renderer inserts are resolved, so
# that a test can look for the text as it was written.
render_table <- function(x) {
  txt <- if (inherits(x, "gt_tbl")) {
    as.character(gt::as_raw_html(x))
  } else {
    paste(as.character(x), collapse = "\n")
  }
  txt <- gsub("&lt;", "<", txt, fixed = TRUE)
  txt <- gsub("&gt;", ">", txt, fixed = TRUE)
  txt <- gsub("&amp;", "&", txt, fixed = TRUE)
  txt <- gsub("‘|’", "'", txt)
  txt <- gsub("“|”", '"', txt)
  txt <- gsub("–|—", "-", txt)
  txt
}
