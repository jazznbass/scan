#' Export scan objects to html or latex
#' 
#' Export creates html files of tables or displays them directly in the viewer
#' pane of rstudio. When applied in rmarkdown/quarto, tables can also be created
#' for pdf/latex output.
#'
#' @param object An scdf or an object exported from a scan function.
#' @param caption Character string with table caption. If left NA (default) a
#'   caption will be created based on the exported object.
#' @param footnote Character string with table footnote. If left NA (default) a
#'   footnote will be created based on the exported object.
#' @param filename String containing the file name. If a filename is given the
#'   output will be written to that file.
#' @param kable_options list with arguments passed to the kable function.
#' @param kable_styling_options list with arguments passed to the kable_styling
#'   function.
#' @param cols Defines which columns are included when exporting an scdf. It is
#'   either a vector of variable names or the string "main" will select the
#'   central variables.
#' @param flip If TRUE, some objects are exported with rows and columns flipped.
#' @param round Integer passed to the digits argument internally used to round
#'   values.
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

.select <- function(df, select) {

  if (identical(select, "none") || 
      identical(select, "") ||
      identical(select, NA) ||
      is.null(select) ||
      identical(select, FALSE)) return(df)
  
  if (!all(select %in% names(df)) && !is.numeric(select)) {
    warning(
      "`select` arguments has variable names that are not included in " ,
      "the output table: valid names are: ", 
      paste(names(df), collapse = ", "), "."
    )
  }
  df <- df[, select]
  if (!is.null(names(select))) {
    if (is.numeric(select)) {
      select <- setNames(names(df)[select], names(select))
    }
    select <- mapply(function(x, y) ifelse(x == "",y,x), names(select), select)
    names(df) <- select
  }
  df
}

.join_kabel <- function(kable_options) {
  
  default_kable <- getOption("scan.export.kable")
  
  tmp <- which(!(names(default_kable) %in% names(kable_options)))
  kable_options <- c(kable_options, default_kable[tmp])
  
  kable_options
} 

.join_kabel_styling <- function(kable_styling_options) {
  
  default_kable_styling <- getOption("scan.export.kable_styling")
  
  tmp <- which(
    !(names(default_kable_styling) %in% names(kable_styling_options))
  )
  
  kable_styling_options <- c(kable_styling_options, default_kable_styling[tmp])
  
  kable_styling_options
} 

.save_export <- function(x, filename) {
  
  
  if (getOption("scan.export.engine") == "kable") {
    kableExtra::save_kable(x, filename, zoom = 2)
  }
  
  if (getOption("scan.export.engine") == "gt") {
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
  if (inherits(x, "kableExtra")) {
    footnote <- gsub("<br>", "\n", footnote)
    out <- kableExtra::footnote(
      x, general = footnote, threeparttable = TRUE,
      footnote_as_chunk = TRUE
    )
  }
}

.create_table <- function(x, 
                          options, 
                          kable_styling_args, 
                          caption = NULL,
                          footnote = NULL,
                          align = NULL,
                          ...) {
  
  if (getOption("scan.export.engine") == "gt") {
    table <- export_table_gt(
      x, 
      title = caption, 
      footnote = footnote,
      ...
    )
    return(table)
  }
  
  rownames(x) <- NULL
  
  if (!is.null(align)) options$align <- align
  
  if (is.null(options$align))  
    options$align <- c("l", rep("c",  ncol(x) - 1))
  
  if (is.null(options$caption))  
    options$caption <- caption
  
  options$x <- x
  table <- do.call(kable, options)
  kable_styling_args$kable_input <- table
  table <- do.call(kable_styling, kable_styling_args)
  
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
  
  out <- do.call(gt::gt, list(data = x))|> gt_apa_style()
  
  if (!is.null(title)) out <- gt::tab_header(out, title = gt::md(title))
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
