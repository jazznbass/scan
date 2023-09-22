#' Convert
#'
#' Converts an scdf object into R code
#'
#' @inheritParams .inheritParams
#' @param file A filename for exporting the syntax.
#' @param study_name Character string. Name of the study object.
#' @param case_name Character string. Name of the scdf objects.
#' @param inline If TRUE, phase definition is in an online version.
#' @param indent Integer. Indentation.
#' @param silent If TRUE, syntax is not printed to the console
#' @return Returns a string (invisible).
#' @keywords io
#' @family io-functions
#' @examples
#' filename <- tempfile()
#' convert(exampleABC, file = filename)
#' source(filename)
#' all.equal(study, exampleABC)
#' unlink(filename)
#' @export
convert <- function(scdf,
                    file = "",
                    study_name = "study",
                    case_name = "case",
                    inline = FALSE,
                    indent = 2,
                    silent = FALSE) {
  check_args(
    by_class(scdf, "scdf")
  )

  if (length(scdf) == 1) case_name <- study_name

  scdf_string <- c()
  
  var_phase <- phase(scdf)
  var_dv <- dv(scdf)
  var_mt <- mt(scdf)

  sindent <- strrep(" ", indent)

  for (i_case in seq_along(scdf)) {

    body_string <- format_body(
      case = scdf[i_case],
      inline,
      indent
    )

    def_string <- format_definition(
      var_dv,
      var_phase,
      var_mt,
      sindent,
      casename = names(scdf)[i_case],
      inline
    )

    phase_design <- format_phase_design(
      inline,
      design = rle(as.character(scdf[[i_case]][[var_phase]])),
      def_string,
      sindent
    )

    scdf_string[i_case] <- paste0(
      case_name, i_case, " <- scdf(",
      body_string,
      phase_design,
      def_string, "\n)"
    )
  }

  scdf_string <- paste0(scdf_string, collapse = "\n\n")

  study_string <- format_study(
    n_cases = length(scdf),
    scdf_attr(scdf),
    case_name,
    indent,
    sindent,
    study_name
  )

  complete_string <- paste(
    scdf_string,
    if (!is.null(study_string)) study_string,
    sep = "\n\n"
  )

  if (!silent) cat(complete_string, "\n")
  if (file != "") cat(complete_string, "\n", file = file)
  
  invisible(complete_string)
}

format_phase_design <- function(inline, design, def_string, sindent) {
  if (!inline) {
    phase_design <- paste0(
      design$values, " = ", design$lengths,
      collapse = ", "
    )
    phase_design <- paste0("phase_design = c(", phase_design, ")")
    phase_design <- paste0(",\n", sindent, phase_design)
  } else {
    phase_design <- paste0(",\n")
  }

  phase_design
}

format_body <- function(case,
                        inline,
                        indent) {

  attr_case <- scdf_attr(case)
  case <- case[[1]]
  var_names <- names(case)[!names(case) %in% attr_case[[opt("phase")]]]
  
  
  vars <- c()

  sindent <- strrep(" ", indent)
  
  for (i in seq_along(var_names)) {
    values <- case[, var_names[i]]

    if (var_names[i] == attr_case[[opt("mt")]]) {
      if (all(seq_along(values) == values)) next
    }

    if (is.numeric(values)) {
      values_string <- paste0(values, collapse = ", ")
    }
    if (!is.numeric(values)) {
      values_string <- paste0('\"', values, '\"', collapse = ", ")
    }

    if (var_names[i] == attr_case[[opt("dv")]] && inline) {
      x <- split(
        case[[attr_case[[opt("dv")]]]], 
        case[[attr_case[[opt("phase")]]]]
      )
      x <- mapply(
        function(x, n) {
          paste0(n, " = ", paste0(x, collapse = ", "), collapse = "")
        },
        x, names(x)
      )

      values_string <- paste0(
        x,
        collapse = paste0(",\n", strrep(" ", indent * 2))
      )
      vars[i] <- paste0(
        var_names[i], " = c(\n", strrep(" ", 2 * indent), values_string, "\n",
        strrep(" ", indent * 1), ")"
      )
    } else {
      values_string <- paste0(
        strwrap(values_string, exdent = 2 * indent),
        collapse = "\n"
      )
      vars[i] <- paste0(
        var_names[i], " = c(\n", sindent, sindent, values_string, "\n",
        sindent, ")"
      )
    }
  }
  vars <- vars[!is.na(vars)]
  paste0("\n", sindent, vars, collapse = ",")
}

format_definition <- function(var_dv,
                              var_phase,
                              var_mt,
                              sindent,
                              casename,
                              inline) {
  def_string <- character(0)
  if (var_dv != "values") {
    def_string <- c(
      def_string,
      paste0(sindent, 'dvar = \"', var_dv, '\"')
    )
  }
  if (var_phase != "phase") {
    def_string <- c(
      def_string,
      paste0(sindent, 'pvar = \"', var_phase, '\"')
    )
  }
  if (var_mt != "mt") {
    def_string <- c(
      def_string,
      paste0(sindent, 'mvar = \"', var_mt, '\"')
    )
  }
  if (!is.null(casename) && !is.na(casename)) {
    def_string <- c(
      def_string,
      paste0(sindent, 'name = \"', casename, '\"')
    )
  }
  def_string <- paste0(def_string, collapse = ", \n")

  if (!inline && def_string != "")
    def_string <- paste0(",\n", def_string)

  def_string
}

format_study <- function(n_cases,
                         attr_scan,
                         case_name,
                         indent,
                         sindent,
                         study_name) {
  con_string <- NULL

  if (n_cases > 1 ||
      !is.null(attr_scan$author) ||
      !is.null(attr_scan$info)) {
    con_string <- paste0(case_name, 1:n_cases, collapse = ", ")
    con_string <- paste0(
      strwrap(con_string, exdent = indent, width = 80, simplify = TRUE),
      collapse = "\n"
    )
    if (!is.null(attr_scan$info)) {
      con_string <- paste0(
        con_string, ", \n", sindent, "info = \"", attr_scan$info, "\""
      )
    }
    if (!is.null(attr_scan$author)) {
      con_string <- paste0(
        con_string, ", \n", sindent, "author = \"", attr_scan$author, "\""
      )
    }
    con_string <- paste0(study_name, " <- c(\n", sindent, con_string, "\n)")
  }
  con_string
}
