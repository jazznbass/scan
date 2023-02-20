#' Convert
#' Converts an scdf object into R code
#'
#' @inheritParams .inheritParams
#' @param file A filename for exporting the syntax.
#' @param study_name Character string. Name of the study object.
#' @param case_name Character string. Name of the scdf objects.
#' @param indent Integer. Indentation.
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
                    indent = 3) {
  

  check_args(by_class(scdf, "scdf"))
  
  scdf_string <- c()
  attr_scan <- attributes(scdf)[[.opt$scdf]]
  
  for (case in seq_along(scdf)) {
    
    dat <- scdf[[case]]
    design <- rle(as.character(scdf[[case]][, attr_scan[[.opt$phase]]]))
    
    phase_design <- paste0(design$values, " = ", design$lengths, collapse=", ")
    phase_design <- paste0("phase_design = c(", phase_design, ")")
    
    var_names <- names(dat)[!names(dat) %in% attr_scan[[.opt$phase]]]
    vars <- c()
    for (i in seq_along(var_names)) {
      values <- dat[, var_names[i]]
      
      if (var_names[i] == attr_scan[[.opt$mt]]) {
        if (all(1:length(values) == values)) next
      }
      
      if (is.numeric(values)) 
        values_string <- paste0(values, collapse = ", ")
      if (!is.numeric(values)) 
        values_string <- paste0('\"', values, '\"', collapse = ", ")
      
      vars[i] <- paste0(var_names[i], " = c(", values_string, ")")
      vars[i] <- paste0(
        strwrap(
          vars[i], 
          exdent = nchar(var_names[i]) + indent + 5,
        ), 
        collapse = "\n"
      )
    }
    
    vars <- vars[!is.na(vars)]
    body_string <- paste0("\n", strrep(" ", indent), vars, collapse = ",")
    def_string <- character(0)
    
    if(attr_scan[[.opt$dv]] != "values") 
      def_string <- c(
        def_string, 
        paste0(strrep(" ", indent), 'dvar = \"', attr_scan[[.opt$dv]], '\"')
      )
    if(attr_scan[[.opt$phase]] != "phase") 
      def_string <- c(
        def_string, 
        paste0(strrep(" ", indent), 'pvar = \"', attr_scan[[.opt$phase]], '\"')
      )
    if(attr_scan[[.opt$mt]] != "mt") 
      def_string <- c(
        def_string, 
        paste0(strrep(" ", indent), 'mvar = \"', attr_scan[[.opt$mt]], '\"')
      )
    if(!is.null(names(scdf)[case]) && !is.na(names(scdf)[case]))
      
      def_string <- c(
        def_string, 
        paste0(strrep(" ", indent), 'name = \"', names(scdf)[case], '\"')
      )
    def_string <- paste0(def_string, collapse =", \n")
    
    if (def_string != "") def_string <- paste0(",\n", def_string)
    
    if (length(scdf) == 1) {
      scdf_string[case] <- paste0(
        study_name, " <- scdf(", 
        body_string, ",\n", 
        strrep(" ", indent), phase_design,
        def_string, "\n)"
      )
    } else {
      scdf_string[case] <- paste0(
        case_name, case, " <- scdf(", 
        body_string, ",\n", 
        strrep(" ", indent), phase_design,
        def_string, "\n)"
      )
    }
 
  }
  
  scdf_string <- paste0(scdf_string, collapse = "\n\n")
  
  if (length(scdf) > 1) {
    con_string <- paste0(case_name, seq_along(scdf), collapse = ", ")
    con_string <- paste0(study_name, " <- c(", con_string, ")")
  } else {
    con_string <- NULL
  }
  
  attr_string <- character(0)
  if (!is.null(attr_scan$info)) {
    info <- attr_scan[[.opt$info]]
    attr_string <-paste0(
      "scdf_attr(", study_name, ", \"info\") <- \"", info, "\""
    ) 
  }
  if (!is.null(attr_scan$author)) {
    author <- attr_scan[[.opt$author]]
    attr_string <- c(
      attr_string, 
      paste0("scdf_attr(", study_name, ", \"author\") <- \"", author, "\"")
    )
  }
  
  attr_string <- paste(attr_string, collapse = "\n")
  
  if (!is.null(con_string)) {
    complete_string <- paste(
      scdf_string,
      con_string,
      attr_string,
      sep = "\n\n"
    )
  } else {
    complete_string <- paste(
      scdf_string,
      attr_string,
      sep = "\n\n"
    )
  }

  cat(complete_string, file = file)
  
}


