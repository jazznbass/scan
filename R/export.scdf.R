#' @rdname export
#' @export
export.scdf <- function(object, 
                        caption = NA, 
                        footnote = NULL, 
                        filename = NA,
                        kable_styling_options = list(), 
                        kable_options = list(),
                        cols, 
                        round = 3, 
                        ...) {
  
  kable_options <- .join_kabel(kable_options)
  kable_styling_options <- .join_kabel_styling(kable_styling_options)
  
  if (is.null(footnote)) {
    if (!is.null(scdf_attr(object, "info"))) 
      footnote <- scdf_attr(object, "info")
    if (!is.null(scdf_attr(object, "author"))) {
      footnote <- paste(footnote, "\nAuthor:", scdf_attr(object, "author"))
    }
  }
  
  N <- cases <- length(object)
  
  if (is.na(caption)) 
    caption <- paste("Single case data frame with", number_word(N), "cases")
  
  if (missing(cols)) {
    cols <- names(object[[1]])
  }
  if (identical(cols, "main")) {
    cols <- c(
      scdf_attr(object, opt("phase")), 
      scdf_attr(object, opt("dv")), 
      scdf_attr(object, opt("mt"))
    )
  }
  
  names(object) <- revise_names(object)
  
  max_row <- max(unlist(lapply(object, nrow)))
  for (i in 1:cases) {
    n_row <- nrow(object[[i]])
    object[[i]][, scdf_attr(object, opt("phase"))] <- as.character(object[[i]][, scdf_attr(object, opt("phase"))])
    if (n_row < max_row) {
      object[[i]][(n_row + 1):max_row, names(object[[i]])] <- ""
    }
  }
  rows <- max_row
  out <- lapply(object[1:cases], function(x) x[1:rows, cols])
  names <- lapply(out, names)
  out <- as.data.frame(out)
  names(out) <- unlist(names[1:cases])
  
  kable_options$align <- rep("c", ncol(out))
  
  spanner <- setNames(vector("list", N), names(object))
  n_vars <- ncol(out) / N
  for(i in 1:N) {
    spanner[[i]] <- ((i - 1) * n_vars + 1) : ((i - 1) * n_vars + n_vars)
  }
  
  table <- .create_table(
    out, 
    kable_options, 
    kable_styling_options, 
    caption = caption,
    footnote = footnote,
    spanner = spanner,
    ...
  )
  
  if (getOption("scan.export.engine") == "kable") {
    case_names <- rep(n_vars, N)
    names(case_names) <- names(object)
    table <- add_header_above(table, case_names)
  }
  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) .save_export(table, filename)
  
  table
}


