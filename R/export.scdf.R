#' @rdname export
#' @export
export.scdf <- function(object, 
                        summary = FALSE,
                        caption = NA, 
                        footnote = NA, 
                        filename = NA,
                        cols, 
                        round = 2, 
                        ...) {
  
  if (summary) {
    sink(nullfile())
    out <- summary(object)
    sink()
    return(export(out))
  }
  
  footnote <- .footnote(footnote,
    if (!is.null(scdf_attr(object, "info"))) 
      scdf_attr(object, "info"),
    if (!is.null(scdf_attr(object, "author"))) 
      paste("Author:", scdf_attr(object, "author"))
  )
  
  N <- cases <- length(object)
  
  if (is.na(caption)) {
    if (N > 1) {
      caption <- paste("Single case data frame with", number_word(N), "cases")
    } else {
      caption <- paste("Single case data frame for case", names(object))
    }
    
  }
      
  if (missing(cols)) {
    cols <- names(object[[1]])
  }
  if (identical(cols, "main")) {
    cols <- c(phase(object), dv(object), mt(object))
  }
  
  names(object) <- revise_names(object)
  
  max_row <- max(unlist(lapply(object, nrow)))
  for (i in 1:cases) {
    n_row <- nrow(object[[i]])
    object[[i]][, phase(object)] <- as.character(object[[i]][, phase(object)])
    if (n_row < max_row) {
      object[[i]][(n_row + 1):max_row, names(object[[i]])] <- ""
    }
  }
  rows <- max_row
  out <- lapply(object[1:cases], function(x) x[1:rows, cols])
  names <- lapply(out, names)
  out <- as.data.frame(out)
  names(out) <- unlist(names[1:cases])
  
  n_vars <- ncol(out) / N
  
  if (N > 1) {
    spanner <- setNames(vector("list", N), names(object))
    for(i in 1:N) {
      spanner[[i]] <- ((i - 1) * n_vars + 1) : ((i - 1) * n_vars + n_vars)
    }
  } else {
    spanner <- NULL
  } 
  
  opts <- options(scan.export.kable = c(
    list(align = rep("c", ncol(out))), 
    getOption("scan.export.kable")
  ))
  on.exit(options(opts), add = TRUE)
  
  
  table <- .create_table(
    out, 
    caption = caption,
    footnote = footnote,
    spanner = spanner,
    ...
  )
  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) .save_export(table, filename)
  
  table
}
