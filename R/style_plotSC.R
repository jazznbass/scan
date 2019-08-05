#' Create styles for single-case data plots
#' 
#' The \code{style_plotSC} function is used to create graphical styles for a single-case plot
#' 
#' 
#' @aliases style_plotSC
#' 
#' @param style Predefined styles.
#' @param ... Further arguments passed to the plot command.
#' 
#' @return Returns a list to be provided or the style argument of the plot function.
#' @return \itemize{ 
#' \item\code{fill} If set, the area under the line is
#' filled with the given color (e.g., \code{fill = "tomato"}). Use the standard
#' R command colors() to get a list of all possible colours. \code{fill} is
#' empty by default.  
#' \item\code{annotations} A list of parameters defining
#' annotations to each data point. This adds the score of each MT to your plot.
#' \itemize{ 
#' \item\code{"pos"} Position of the annotations: 1 = below, 2 =
#' left, 3 = above, 4 = right.  
#' \item\code{"col"} Color of the annotations.
#' \item\code{"cex"} Size of the annotations.
#' \item\code{"round"} Rounds the values to the specified decimal.} 
#' \item\code{annotations = list(pos = 3, col =
#' "brown", round = 1)} adds scores rounded to one decimal above the data point
#' in brown color to the plot.
#' \item\code{"lwd"} Width of the plot line. Default is \code{lwd = 2}.
#' \item\code{"pch"} Point type. Default is \code{pch = 17} (triangles). Other options
#' are for example: 16 (filled circles) or "A" (uses the letter A).
#' \item\code{"main"} Main title of the plot.
#' \item\code{"mai"} Sets the margins of the plot.
#' \item\code{"bty"} Shape of the frame surrounding the inner plot
#' \item\code{"fill.bg"} Backgroundcolor of the plot.
#' \item\code{"grid"} Color of a grid.
#' \item\code{"text.ABlag"} Text displayed between phases.
#' \item\code{"cex.axis"} Size of the axis annotations
#' \item\code{"las"} Orientation of the axis annotations
#' \item\code{"col.lines"} Color of the lines
#' \item\code{"col.dots"} Color of the dots
#' \item\code{"col.seperator"} Color of the phase seperating lines
#' \item\code{"col.bg"} Color of the outer plot
#' \item\code{"col"} General color setting for the plot
#' \item\code{"col.text"} Color of all labels of the plot.
#' }
#' @details \code{style_plotSC("")} will return a list of predefined styles.
#' Predefined styles can be combined \code{style_plotSC(style = c("grid2", "tiny"))} 
#' where settings of a latter style overwrite settings of the former.
#' Additional style paramters are set following the style argument and can be combined with those:
#' \code{style_plotSC(style = "grid2", fill = "grey50", pch = 18)}. 
#' @author Juergen Wilbert
#' @seealso \code{\link{plot.scdf}}
#' @examples
#' newstyle <- style_plotSC(style = "default")
#' newstyle$text.ABlag <- c("START","END")
#' newstyle$col.dots <- ""
#' newstyle$annotations <- list(cex = 0.6, col = "grey10", offset = 0.4)
#' plot(exampleABC, style = newstyle)
#' 
#' @export

style_plotSC <- function(style = "default", ...) {
  new <- list(...)
  if (identical(style, "")) {
    cat("Available styles: \n")
    cat(paste(names(.opt$style), collapse = ", "), "\n")
    return(invisible(NULL))
  }
  if (!(all(style %in% names(.opt$style)))) {
    cat("Style '", paste(style), "' is unknown.\n", sep = "")
    cat("Available styles: \n")
    cat(paste(names(.opt$style), collapse = ", "), "\n")
    return(invisible(NULL))
  }
  
  styles <- list()
  for(i in rev(style)) styles <- c(styles, .opt$style[[i]])

  out <- c(styles, .opt$style$default)
  out <- out[unique(names(out))]
  if(is.list(new)) {
    out <- c(new, out)
    out <- out[unique(names(out))]
  }
  invisible(out)
}

#' @rdname style_plotSC
style.plotSC <- function(...) {
  warning(.opt$function_deprecated_warning, "\nPlease use style_plotSC instead of style.plotSC")
  style.plotSC(...)
}