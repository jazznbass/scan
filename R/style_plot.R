#' Create styles for single-case data plots
#' 
#' The \code{style_plot} function is used to create graphical styles for a single-case plot
#' 
#' @param style A character string or a vector of character strings with predefined styles.
#' @param ... Further arguments passed to the plot command.
#' 
#' @return Returns a list to be provided for the style argument of the \code{\link{plot.scdf}} function.
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
#' \item\code{"names"} A list of parameters defining the depiction of phase names (e.g. \code{names = list(cex = 0.8, col = "red", side = 1)}: cex for size, col for color, and side for position). See \code{\link{mtext}} for more details. 
#' \item\code{"lwd"} Width of the plot line. Default is \code{lwd = 2}.
#' \item\code{"pch"} Point type. Default is \code{pch = 17} (triangles). Other options
#' are for example: 16 (filled circles) or "A" (uses the letter A).
#' \item\code{"main"} Main title of the plot.
#' \item\code{"mai"} Sets the margins of the plot.
#' \item\code{"bty"} Shape of the frame surrounding the inner plot
#' \item\code{"fill.bg"} Background color of the plot.
#' If a vector is provided, these colors will be assigned to phases 
#' (each phase name becomes a color).
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
#' @details \code{style_plot("")} will return a list of predefined styles.
#' Predefined styles can be combined \code{style_plot(style = c("grid2", "tiny"))} 
#' where settings of a latter style overwrite settings of the former.
#' Additional style paramters are set following the style argument and can be combined with those:
#' \code{style_plot(style = "grid2", fill = "grey50", pch = 18)}. 
#' @author Juergen Wilbert
#' @seealso \code{\link{plot.scdf}}
#' @examples
#' newstyle <- style_plot(style = "default")
#' newstyle$text.ABlag <- c("START","END")
#' newstyle$col.dots <- ""
#' newstyle$annotations <- list(cex = 0.6, col = "grey10", offset = 0.4)
#' newstyle$names <- list(cex = 0.8, col = "blue", side = 1, adj = 1, line = -1, at = 31)
#' newstyle$fill.bg <- c("grey99", "grey95", "grey90")
#' plot(exampleABC, style = newstyle, main = "Example Plot")
#' 
#' @export
style_plot <- function(style = "default", ...) {
  new <- list(...)
  if (identical(style, "")) {
    cat("Available styles: \n")
    cat(paste(names(opt("style")), collapse = ", "), "\n")
    return(invisible(NULL))
  }
  if (!(all(style %in% names(opt("style"))))) {
    cat("Style '", paste(style), "' is unknown.\n", sep = "")
    cat("Available styles: \n")
    cat(paste(names(opt("style")), collapse = ", "), "\n")
    return(invisible(NULL))
  }
  
  styles <- list()
  for(i in rev(style)) styles <- c(styles, opt("style")[[i]])

  out <- c(styles, opt("style")$default)
  out <- out[unique(names(out))]
  if(is.list(new)) {
    out <- c(new, out)
    out <- out[unique(names(out))]
  }
  invisible(out)
}
