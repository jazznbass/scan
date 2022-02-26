#' Covert
#' Converts an scdf object to a text syntax
#'
#' @inheritParams .inheritParams
#' @param file A filename for exporting the syntax.
#' @param study_name Character string. Name of the single case study name.
#' @examples 
#' filename <- tempfile()
#' convert(exampleABC, file = filename)
#' source(filename)
#' all.equal(study, exampleABC)
#' unlink(filename)
#' @export

convert <- function(scdf, file = "", study_name = "study") {
  
  scdf_string <- c()
  attr_scan <- attributes(scdf)[[.opt$scdf]]
  
  for (case in seq_along(scdf)) {
    
    dat <- scdf[[case]]
    design <- rle(as.character(scdf[[case]][, attr_scan[[.opt$phase]]]))
    phase_design <- paste0(design$values, " = ", design$lengths, collapse = ", ")
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
    }
    vars <- vars[!is.na(vars)]
    
    body_string <- paste0("\n   ", vars, collapse = ",")
    
    
    def_string <- character(0)
    if(attr_scan[[.opt$dv]] != "values") 
      def_string <- c(def_string, paste0('   dvar = \"', attr_scan[[.opt$dv]], '\"'))
    if(attr_scan[[.opt$phase]] != "phase") 
      def_string <- c(def_string, paste0('   pvar = \"', attr_scan[[.opt$phase]], '\"'))
    if(attr_scan[[.opt$mt]] != "mt") 
      def_string <- c(def_string, paste0('   mvar = \"', attr_scan[[.opt$mt]], '\"'))
    
    if(!is.null(names(scdf)[case]))
      def_string <- c(def_string, paste0('   name = \"', names(scdf)[case], '\"'))
    
    def_string <- paste0(def_string, collapse =", \n")
    
    if (def_string != "") def_string <- paste0(",\n", def_string)
    
    scdf_string[case] <- paste0(
      "case", case, " <- scdf(", 
      body_string, ", \n   ", phase_design,
      def_string, "\n)"
    )
  }
  
  scdf_string <- paste0(scdf_string, collapse = "\n\n")
  
  con_string <- paste0("case", seq_along(scdf), collapse = ", ")
  con_string <- paste0(study_name, " <- c(", con_string, ")")
  
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
  
  complete_string <- paste(
    scdf_string,
    con_string,
    attr_string,
    sep = "\n\n"
  )
  
  cat(complete_string, file = file)
  
}
