#' Export scan objects to html or latex
#' 
#' Export creates html files of tables or displays them directly in the viewer
#' pane of rstudio. When applied in rmarkdown/quarto, tables can also be created
#' for pdf/latex output.
#' 
#' The function uses either the gt or the kableExtra package to create the
#' tables. The default engine is gt; in a document knitted to latex, kableExtra
#' is used instead, because gt tables receive neither a number nor a label
#' there, and for Word output gt is used even when kableExtra is set, because
#' kableExtra can not write Word tables. The engine can be set via the option
#' `scan.export.engine`. Additional
#' options for kable and kable_styling can be set via the options
#' `scan.export.kable` and `scan.export.kable_styling`.
#' The default options can be viewed and modified via
#' `options("scan.export.kable")` and
#' `options("scan.export.kable_styling")`.
#'
#' @param object An scdf or an object exported from a scan function.
#' @param caption Character string with table caption. If left NA (default) a
#'   caption will be created based on the exported object.
#' @param footnote Character string with table footnote. Several strings are
#'   combined into a footnote of several lines. If left NA (default) a footnote
#'   will be created based on the exported object. `NULL` or `""` suppress the
#'   footnote.
#' @param filename String containing the file name. If a filename is given the
#'   output will be written to that file.
#' @param cols Defines which columns are included when exporting an scdf. It is
#'   either a vector of variable names or the string "main" will select the
#'   central variables.
#' @param flip If TRUE, some objects are exported with rows and columns flipped.
#' @param round Integer passed to the digits argument used to round values.
#' @param decimals Decimal places that are reported.
#' @param select A character vector containing the names of the variables to be
#'   included. If the vector is named, the variables will be renamed
#'   accordingly.
#' @param summary If TRUE, exports the summary of an `scdf`.
#' @param ... Further Arguments passed to internal functions.
#' @return  Returns or displays a specially formatted html (or latex) file.
#' @export

export <- function (object, ...) {
  UseMethod("export")
}


.footnote <- function(footnote, ...) {
  if (is.null(footnote)) return(NULL)
  if (length(footnote) == 1L && is.na(footnote)) return(c(...))
  footnote
}


.select <- function(df, select) {

  if (identical(select, "none") || 
      identical(select, "") ||
      identical(select, NA) ||
      is.null(select) ||
      identical(select, FALSE)) return(df)
  
  if (!is.numeric(select)) {
    unknown <- setdiff(select, names(df))
    if (length(unknown) > 0) {
      abort(
        "Argument 'select' names variables that are not in the table: ",
        paste0("'", unknown, "'", collapse = ", "), 
        ". Available are: ", paste(names(df), collapse = ", "), "."
      )
    }
  }
  
  if (is.numeric(select) && any(select < 1 | select > ncol(df))) {
    abort("Argument 'select' has column numbers outside the table ",
          "(1 to ", ncol(df), ").")
  }
  
  old_names <- names(df)
  
  df <- df[, select, drop = FALSE]
  
  if (!is.null(names(select))) {
    if (is.numeric(select)) {
      select <- setNames(old_names[select], names(select))
    }
    new_names <- names(select)
    new_names[new_names == ""] <- select[new_names == ""]
    names(df) <- new_names
  }
  df
}

# The engine to use. In a document knitted to latex, gt tables carry no
# number and no label, so kable is used there even when gt is set.
.export_engine <- function() {
  engine <- getOption("scan.export.engine")
  
  if (!identical(engine, "gt") && !identical(engine, "kable")) {
    abort("Option 'scan.export.engine' must be either 'gt' or 'kable'.")
  }
  
  if (identical(engine, "gt") && isTRUE(knitr::is_latex_output())) {
    return("kable")
  }
  
  if (identical(engine, "kable") && isTRUE(knitr::pandoc_to("docx"))) {
    if (!isTRUE(.opt$notified_word_engine)) {
      notify("Word output: the kable engine can not produce usable tables. ",
             "The gt engine is used instead.")
      .opt$notified_word_engine <- TRUE
    }
    return("gt")
  }
  
  engine
}

# Text for a latex table: pdflatex does not know the greek letters and the
# superscript two, and it renders neither html tags nor markdown, so the
# characters are spelled out and the markup is removed.
.latex_plain <- function(x) {
  if (is.null(x)) return(x)
  
  subs <- c(
    "\u03A6\u00b2" = "Phi-squared", "\u03A6" = "Phi",
    "\u03C7\u00b2" = "Chi-squared", "\u03C7" = "Chi",
    "R\u00b2" = "R-squared", "X\u00b2" = "X-squared", "\u00b2" = "^2"
  )
  
  replace_one <- function(v) {
    if (!is.character(v)) return(v)
    
    v <- gsub("<br ?/?>", " ", v)
    v <- gsub("<[^>]*>", "", v)
    v <- gsub("\\*\\*(.+?)\\*\\*", "\\1", v)
    v <- gsub("\\*(.+?)\\*", "\\1", v)
    
    for (i in seq_along(subs)) {
      v <- gsub(names(subs)[i], subs[i], v, fixed = TRUE)
    }
    v
  }
  
  if (is.data.frame(x)) {
    x[] <- lapply(x, replace_one)
    return(x)
  }
  
  replace_one(x)
}

# NaN and Inf are not results; they are shown as empty cells in both engines
.blank_non_finite <- function(x) {
  num <- vapply(x, is.numeric, logical(1))
  x[num] <- lapply(x[num], function(v) {
    v[!is.finite(v)] <- NA
    v
  })
  x
}

.save_export <- function(x, filename) {
  
  engine <- .export_engine()
  
  if (identical(engine, "kable")) {
    kableExtra::save_kable(x, filename, zoom = 2)
  } else {
    gt::gtsave(x, filename)
  }
  
}

.add_footnote <- function(x, footnote) {
  
  if (length(footnote) > 1) {
    footnote <- paste0(
      footnote, 
      collapse = getOption("scan.export.footnote.collapse")
    ) |> paste0(".")
  }
  
  if (!inherits(x, "kableExtra") && !inherits(x, "knitr_kable")) return(x)
  
  format <- attr(x, "format")
  if (is.null(format)) {
    format <- if (any(grepl("<table", x, fixed = TRUE))) "html" else "latex"
  }
  html <- identical(format, "html")
  
  if (!html) footnote <- gsub("<br>", "\n", footnote)
  
  kableExtra::footnote(
    x, general = footnote, threeparttable = TRUE,
    footnote_as_chunk = TRUE, escape = !html
  )
}

.create_table <- function(x, 
                          options = getOption("scan.export.kable"), 
                          kable_styling_args = getOption("scan.export.kable_styling"), 
                          caption = NULL,
                          footnote = NULL,
                          align = NULL,
                          decimals = NULL,
                          row_group = NULL,
                          spanner = NULL,
                          ...) {
  
  engine <- .export_engine()
  
  if (identical(engine, "gt")) {
    table <- export_table_gt(
      x, 
      title = caption, 
      footnote = footnote,
      decimals = decimals,
      row_group = row_group,
      spanner = spanner,
      ...
    )
    return(table)
  }
  
  rownames(x) <- NULL
  
  x <- .blank_non_finite(x)
  
  format <- options$format
  if (is.null(format)) format <- getOption("knitr.table.format")
  if (is.null(format)) {
    format <- if (isTRUE(knitr::is_latex_output())) "latex" else "html"
  }
  
  if (identical(format, "latex")) {
    x <- .latex_plain(x)
    names(x) <- .latex_plain(names(x))
    caption <- .latex_plain(caption)
    footnote <- .latex_plain(footnote)
  }
  
  if (!is.null(align)) options$align <- align
  
  if (is.null(options$align)) options$align <- c("l", rep("c",  ncol(x) - 1))
  
  if (!is.null(decimals)) {
    num <- vapply(x, is.numeric, logical(1))
    x[num] <- lapply(x[num], function(v) {
      out <- formatC(v, format = "f", digits = decimals)
      out[is.na(v)] <- NA
      out
    })
  }
    
  if (is.null(options$caption)) options$caption <- caption
  
  options$x <- x
  table <- do.call(kable, options)
  kable_styling_args$kable_input <- table
  table <- do.call(kable_styling, kable_styling_args)
  
  for (i in seq_along(row_group)) {
    table <- pack_rows(
      table, names(row_group)[i], 
      min(row_group[[i]]), max(row_group[[i]]), 
      indent = FALSE
    )
  }
  
  # a spanner is given as column positions; add_header_above() wants widths
  if (!is.null(spanner)) {
    spanner <- spanner[order(vapply(spanner, min, numeric(1)))]
    header <- c()
    pos <- 1
    for (i in seq_along(spanner)) {
      from <- min(spanner[[i]])
      to <- max(spanner[[i]])
      if (from > pos) header <- c(header, setNames(from - pos, " "))
      header <- c(header, setNames(to - from + 1, names(spanner)[i]))
      pos <- to + 1
    }
    if (pos <= ncol(x)) header <- c(header, setNames(ncol(x) - pos + 1, " "))
    table <- add_header_above(table, header)
  }
  
  if (!is.null(footnote)) {
    if (!identical(footnote, NA) && !identical(footnote, ""))
      table <- .add_footnote(table, footnote)
  }
  
  table
}

###### gt ####

export_table_gt <- function(x, 
                            title = NULL, 
                            footnote = NULL, 
                            spanner = NULL,
                            row_group = NULL,
                            rownames = FALSE,
                            cols_label = NULL,
                            decimals = NULL,
                            prefix_title = getOption("scan.export.title.prefix"),
                            fmt_markdown = FALSE,
                            ...) {
          
  
  while(TRUE) {
    id <- which(duplicated(names(x)))
    if (length(id) == 0) break 
    names(x)[id] <- paste0(" ", names(x)[id], " ")
    
  }
  
  if (!is.null(title) && title != "") title <- paste0("*", title, "*")
  if (!is.null(footnote) && 
      !identical(footnote, "") && 
      !identical(footnote, NA)) {
    footnote <- paste0(
      "*Note.* ", 
      paste0(footnote, collapse = getOption("scan.export.footnote.collapse")), 
      "."
    )
  }
  
  if (!inherits(x, "data.frame")) {
    x <- as.data.frame(x)
    rownames(x) <- NULL
  }
  if (rownames && !is.null(rownames(x))) x <- cbind(" " = rownames(x), x)
  
  x <- .blank_non_finite(x)
  
  out <- do.call(gt::gt, list(data = x))|> gt_apa_style()
  
  if (!is.null(title)) {
    if (!is.null(prefix_title)) {
      out <- gt::tab_header(out, title = prefix_title, subtitle = gt::md(title))
    } else {
      out <- gt::tab_header(out, title = gt::md(title))
    }
      
  }
  if (!is.null(row_group)) {
    for(i in length(row_group):1)
      out <- gt::tab_row_group(
        out, label = names(row_group)[i], rows = row_group[[i]]
      )
    for(i in length(row_group):1)  
      out <- gt::tab_style(
        out, style = gt::cell_text(align = "center"),
        locations = gt::cells_row_groups(groups = names(row_group)[i])
      )
  }
  if (!is.null(spanner)) {
    for(i in seq_along(spanner)) {
      out <- gt::tab_spanner(
        out, 
        label = names(spanner)[i], 
        columns = spanner[[i]]
      )  
    }
  }
  
  if (!is.null(cols_label)) out <- gt::cols_label(out, .list = cols_label)
  if (!is.null(footnote) && !identical(footnote, "") && !identical(footnote, NA)) 
    out <- gt::tab_footnote(out, gt::md(footnote))
  if (!is.null(decimals)) out <- gt::fmt_number(out, decimals = decimals)
  out <- gt::sub_missing(out, missing_text = "")
  if (fmt_markdown) out <- gt::fmt_markdown(out, columns = gt::everything())
  out
}

gt_apa_style <- function(gt_tbl) {
  gt_tbl  |> 
    gt::tab_options(
      table.border.bottom.color = "white",
      #table.border.bottom.width = 3,
      
      table.border.top.color = "white",
      #table.border.top.width = 3,
      
      table_body.border.bottom.color = "black",
      table_body.border.bottom.width = 3,
      
      table_body.border.top.color = "black",
      table_body.border.top.width = 3,
      
      table_body.hlines.width = 0,
      
      heading.align = "left",
      heading.border.bottom.width = 3,
      heading.border.bottom.color = "black",
      heading.title.font.size = "100%",
      heading.subtitle.font.size = "100%",
      column_labels.border.bottom.width = 2,
      column_labels.border.bottom.color = "black",
      column_labels.border.top.width = 3,
      column_labels.border.top.color = "black",
      
      row_group.border.bottom.color = "white",
      row_group.border.bottom.style = NULL,
      row_group.border.bottom.width = NULL
      
    )  |> 
    gt::opt_table_font(font = "times") |> 
    gt::cols_align(align = "center") |> 
    gt::cols_align(align = "left", columns = 1)
}
