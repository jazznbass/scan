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

  scdf_string <- c()
  attr_scan <- attributes(scdf)[[opt("scdf")]]
  var_phase <- attr_scan[[opt("phase")]]
  var_dv <- attr_scan[[opt("dv")]]
  var_mt <- attr_scan[[opt("mt")]]

  sindent <- strrep(" ", indent)
  
  for (case in seq_along(scdf)) {
    dat <- scdf[[case]]
    design <- rle(as.character(dat[[var_phase]]))

    # phase_design
    if (!inline) {
      phase_design <- paste0(
        design$values, " = ", design$lengths,
        collapse = ", "
      )
      phase_design <- paste0("phase_design = c(", phase_design, ")")
    } else {
      phase_design <- NULL
    }

    var_names <- names(dat)[!names(dat) %in% var_phase]
    vars <- c()
    for (i in seq_along(var_names)) {
      values <- dat[, var_names[i]]

      if (var_names[i] == var_mt) {
        if (all(1:length(values) == values)) next
      }

      if (is.numeric(values)) {
        values_string <- paste0(values, collapse = ", ")
      }
      if (!is.numeric(values)) {
        values_string <- paste0('\"', values, '\"', collapse = ", ")
      }

      if (var_names[i] == var_dv && inline) {
        x <- split(dat[[var_dv]], dat[[var_phase]])
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
          strwrap(values_string, exdent = 2 * indent), collapse = "\n"
        )
        vars[i] <- paste0(
          var_names[i], " = c(\n", sindent, sindent, values_string, "\n", 
          sindent, ")"
        )
        #vars[i] <- paste0(
        #  strwrap(
        #    vars[i],
        #    exdent = nchar(var_names[i]) + indent + 5,
        #  ),
        #  collapse = "\n"
        #)
      }
    }

    vars <- vars[!is.na(vars)]
    body_string <- paste0("\n", sindent, vars, collapse = ",")
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

    if (!is.null(names(scdf)[case]) && !is.na(names(scdf)[case])) {
      def_string <- c(
        def_string,
        paste0(sindent, 'name = \"', names(scdf)[case], '\"')
      )
    }
    def_string <- paste0(def_string, collapse = ", \n")

    if (inline) {
      phase_design <- paste0(",\n")
    } else {
      if (def_string != "") def_string <- paste0(",\n", def_string)
      phase_design <- paste0(",\n", sindent, phase_design)
    }

    if (length(scdf) == 1) case_name <- study_name

    scdf_string[case] <- paste0(
      case_name, case, " <- scdf(",
      body_string,
      phase_design,
      def_string, "\n)"
    )
  }

  scdf_string <- paste0(scdf_string, collapse = "\n\n")

  # study <- c(...)
  if (length(scdf) > 1 || 
      !is.null(attr_scan$author) || 
      !is.null(attr_scan$info)) {
    con_string <- paste0(case_name, seq_along(scdf), collapse = ", ")
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

  } else {
    con_string <- NULL
  }

  if (!is.null(con_string)) {
    complete_string <- paste(
      scdf_string,
      con_string,
      #attr_string,
      sep = "\n\n"
    )
  } else {
    complete_string <- paste(
      scdf_string,
      #attr_string,
      sep = "\n\n"
    )
  }

  if (!silent) {
    cat(
      #"# This is an automatically generated syntax\n\n",
      complete_string, 
      "\n", 
      file = file
    )
  }
  invisible(complete_string)
}
