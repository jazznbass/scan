#' Print an scdf
#'
#' @param x An scdf object
#' @param cases Number of cases to be printed. "fit" fits the number to the current screen width.
#' @param rows Number of rows to be printed. 
#' @param cols Columns to be printed. "Main" only prints the dependent, measurement-time and phase variable.
#' @param long Logical. If TRUE cases are printed in one by a time.
#' @param digits Number of digits.
#' @param ... Further arguments passed to the print function. 
#' @details Print options for scdf objects could be set globally:
#' option(scan.print.cases = "all"), 
#' option(scan.print.rows = 10), 
#' option(scan.print.cols = "main"),
#' option(scan.print.long = TRUE),
#' option(scan.print.digits = 0),
#' option(scan.print.scdf.name = FALSE)
#' @export
print.scdf <- function(x, 
  cases  = getOption("scan.print.cases"), 
  rows   = getOption("scan.print.rows"), 
  cols   = getOption("scan.print.cols"), 
  long   = getOption("scan.print.long"), 
  digits = getOption("scan.print.digits"), ...) {
  
  row.names <- FALSE
  N <- length(x)

  names(x) <- .case.names(names(x), N)

  if (identical(cases, "all")) cases <- N
  if (is.numeric(cases) && cases > N) cases <- N
  if (N == 1) cat("#A single-case data frame with one case\n\n")
  if (N > 1)  cat("#A single-case data frame with", N, "cases\n\n")
  
  if (identical(cols, "main"))
    cols <- c(attr(x, .opt$dv), attr(x, .opt$phase), attr(x, .opt$mt))
  
  if (!identical(cols, "all"))
    for(i in 1:N) x[[i]] <- x[[i]][, cols]
  
  if(getOption("scan.print.scdf.name"))
    for(i in 1:N) 
      names(x[[i]])[1] <- paste0(names(x)[i], ": ", names(x[[i]])[1])
  
  if(identical(cases,"fit")) {
    tmp.width <- cumsum(lapply(x, function(case) nchar(paste0(names(case), collapse = " ")) + 3))
    cases <- length(which(getOption("width") >= tmp.width))
  }
  
  max.row <- max(unlist(lapply(x, nrow)))
  
  if(!long) {
      for(i in 1:cases) {
      n.row <- nrow(x[[i]])
      
      # round and change to character
      for(j in names(x[[i]])) {
        if(is.numeric(digits) && is.numeric(x[[i]][, j]))
            x[[i]][, j] <- round(x[[i]][, j], digits)
        x[[i]][, j] <- as.character(x[[i]][, j])
      }
  
      if (n.row < max.row)
         x[[i]][(n.row + 1):max.row, names(x[[i]])] <- ""
    }
  }
  if (rows == "all") long <- TRUE
  if (!long) {
    if (max.row < rows) 
      rows <- max.row
    out <- lapply(x[1:cases], function(x) x[1:rows, ])
    if (cases > 1)
      out <- lapply(out, 
        function(x) {
          x$"|" <- "|"
          x
        }
      )
    
    names <- lapply(out, names)
    out <- as.data.frame(out)
    names(out) <- unlist(names[1:cases])
    print(out, row.names = row.names, ...)
  }
  if (long) {
    for(case in 1:N) {
      print(x[[case]], row.names = row.names, digits = digits, ...)
      cat("\n")
    }
  }
  
  if (max.row > rows) cat("# ... up to", max.row-rows, "more rows\n")
  if ((N - cases)  > 1) cat("# ", N - cases, "more cases\n")
  if ((N - cases) == 1) cat("# One more case\n")
  
}
