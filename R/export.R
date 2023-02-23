#' Export scan objects to html or latex
#'
#' This function is in an experimental status. Export creates html files of
#' tables or displays them directly in the viewer pane of rstudio. When applied
#' in rmarkdown/quarto, tables can also be created for pdf/latex output.
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
#' @param select A character vector containing the names of the variables to be
#'   included. If the vector is named, the variables will be renamed
#'   accordingly.
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
    warning("`select` arguments has variable names that are not included in " ,
            "the output table: valid names are: ", 
            paste(names(df), collapse = ", "), ".")
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
  
  tmp <- which(!(names(default_kable_styling) %in% names(kable_styling_options)))
  kable_styling_options <- c(kable_styling_options, default_kable_styling[tmp])
  
  kable_styling_options
} 

