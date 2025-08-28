#' @rdname export
#' @export
export.scdf_summary <- function(object, 
                                caption = NA, 
                                footnote = NA, 
                                filename = NA,
                                round = 2, 
                                ...) {
  
  if (is.na(footnote)) {
    footnote <- c(
      if (!is.null(scdf_attr(object, "info"))) scdf_attr(object, "info"),
      if (!is.null(scdf_attr(object, "author"))) paste("Author:", scdf_attr(object, "author"))
    )
    
    names <- names(object[[1]])
    id_dv <- which(names == dv(object))
    id_phase <- which(names == phase(object))
    id_mt <- which(names == mt(object))
    names[id_phase] <- paste(names[id_phase], "(phase variable)")
    names[id_mt] <- paste(names[id_mt], "(measurement-time variable)")
    names[id_dv] <- paste(names[id_dv], "(dependent variable)")
    str_vars <- paste(names[c(
      id_dv, id_phase, id_mt, (1:length(names))[-c(id_dv, id_phase, id_mt)]
    )], collapse = "<br>")
    footnote <- c(footnote, paste0("<br><br><b>Variable names:</b><br>", str_vars, collapse = "<br>")
    )
  }
  
  N <- cases <- length(object)
  
  if (is.na(caption)) {
    caption <- if(N > 1) {
      paste("A single-case data frame with", number_word(N), "cases")
    } else {
      paste("A single-case data frame with one case")
    }
  }
  
  designs <- lapply(object, function(x) {
    paste0(
      rle(as.character(x[[phase(object)]]))$values, "(",
      rle(as.character(x[[phase(object)]]))$lengths, ")",
      collapse = "-")
  })
  
  rows <- lapply(object, nrow)
  
  out <- data.frame(
    Case = format(revise_names(object), justify = "left")[1:min(N, 10)],
    Measurements = unname(unlist(rows))[1:min(N, 10)], 
    Design = unname(unlist(designs))[1:min(N, 10)],
    check.names = FALSE
  )
  
  opts <- options()
  options(scan.export.kable = c(
    list(align = rep("c", ncol(out))), 
    getOption("scan.export.kable")
  ))
  
  table <- .create_table(
    out, 
    caption = caption,
    footnote = footnote,
    ...
  )
  
  # finish ------------------------------------------------------------------
  
  if (!is.na(filename)) .save_export(table, filename)
  
  options(opts)
  
  table
}
