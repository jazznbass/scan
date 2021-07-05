#' Plot single-case data
#' 
#' This function provides a plot of a single-case or multiple
#' single-cases.
#'
#' @inheritParams .inheritParams
#' @return An scdf plot object that creates a plot when printed.
#' @export

scplot <- function(data) {

  warning(.opt$function_experimental_warning)
  
  data <- .prepare_scdf(data)

  theme <- modifyList(
    .opt$scplot_themes$default, 
    .opt$scplot_themes[[getOption("scan.scplot.theme")]], 
    keep.null = TRUE
  )
  
  out <- list(
    scdf = data,
    dvar = scdf_attr(data, .opt$dv), 
    pvar = scdf_attr(data, .opt$phase), 
    mvar = scdf_attr(data, .opt$mt),
    theme = theme,
    title = NULL,
    caption = NULL,
    xaxis = list(lim = NULL, inc = 1),
    yaxis = list(lim = NULL),
    xlabel = NULL,
    ylabel = NULL,
    lines = NULL, 
    marks = NULL, 
    texts = NULL,
    arrows = NULL,
    phase_names = NULL,
    legend = NULL,
    case_names = list(labels = .case_names(names(data), length(data)))
  )
  class(out) <- "scplot"
  out
}



#' @rdname scplot
#' @param theme A character string with a predefined graphical theme. 
#' Possible values: default, yaxis, tiny, small, big, chart, ridge, 
#' annotate, grid, grid2, dark, nodots, sienna, phase_color, phase_shade
#' 
#' @export
add_theme <- function(object, theme, ...) {
  
  themes <- c(theme, ...)
  
  if (!(all(themes %in% names(.opt$scplot_themes)))) {
    stop("Unknown theme template.")
  }

  for(i in themes) {
    object$theme <- modifyList(object$theme, .opt$scplot_themes[[i]], keep.null = TRUE) 
  }

  object
  
}

#' @rdname scplot
#' @param ... various style parameter
#' 
#' @export
set_theme_element <- function(object, ...) {
  
  object$theme <- modifyList(object$theme, list(...), keep.null = TRUE)
  object
}



#' @rdname scplot
#' @param stat A character string defining a line or curve to be
#' plotted. Possible values: "median", "mean", "trend", "trendA", 
#' "trendA_bisplit", "trendA_trisplit", "maxA", "minA", "meanA", "medianA", 
#' "plm", "movingMean", "movingMedian", "loreg"
#' @param width A number defining the line width
#' @param colour A character string or a number defining the color of an element.
#' @param size A number deifning the size of an element.
#' @param type A character string with the line type: "solid", "dashed", "dotted"
#' @export
add_statline <- function(object, stat, colour = NULL, 
                         width = NULL, type = NULL,  variable = ".dvar", ...) {
  
  lines <- list(stat = stat, col = colour, lwd = width, lty = type, variable = variable, ...)
  object$lines <- c(object$lines, list(lines))
  object
  
}

#' @rdname scplot
#' @export
set_xlabel <- function(object, label, colour, size) {
  
  if (!missing(colour)) object$theme$col.xlab <- colour
  if (!missing(size)) object$theme$cex.xlab <- size
  if (!missing(label)) object$xlabel <- label

  object
}

#' @rdname scplot
#' @param orientation of the label: 0 = vertical; 1 = horizontal
#' @export
set_ylabel <- function(object, label, colour, size, line, orientation) {
  
  if (!missing(orientation)) {
    if (orientation %in% c("h", "horizontal")) orientation <- 1
    if (orientation %in% c("v", "vertical")) orientation <- 0
    object$theme$ylab.orientation <- orientation
  }
  
  if (!missing(colour)) object$theme$col.ylab <- colour
  if (!missing(size)) object$theme$cex.ylab <- size
  if (!missing(line)) object$theme$vjust.ylab <- line
  if (!missing(label)) object$ylabel <- label
  
  object
}

#' @rdname scplot
#' @param limits Lower and upper limits of the axis (e.g., \code{limits = c(0,
#' 20)} sets the axis to a scale from 0 to 20). With multiple single-cases
#' you can use \code{limits = c(0, NA)} to scale the axis from 0 to the maximum
#' of each case. \code{limits} is not set by default, which makes \code{scan} set
#' a proper scale based on the given data.
#' @param increment An integer. Increment of the x-axis. 1 :each mt value will be printed, 2 : every other value, 3 : every third values etc.
#' @export
set_xaxis <- function(object, limits, increment, increment_from, 
                      colour, size, line, positions) {
  
  if (!missing(colour)) object$theme$col.xaxis <- colour
  if (!missing(size)) object$theme$cex.xaxis <- size
  if (!missing(line)) object$theme$vjust.xlab <- line
  
  if (!missing(limits)) object$xaxis$lim <- limits
  if (!missing(increment)) object$xaxis$inc <- increment
  if (!missing(increment_from)) object$xaxis$inc_from <- increment_from
  if (!missing(positions)) object$xaxis$pos <- positions
  
  object
}

#' @rdname scplot
#' @export
set_yaxis <- function(object, limits, colour, size, 
                      increment, increment_from, positions) {
  
  if (!missing(colour)) object$theme$col.yaxis <- colour
  if (!missing(size)) object$theme$cex.yaxis <- size

  if (!missing(limits)) object$yaxis$lim <- limits
  if (!missing(increment)) object$yaxis$inc <- increment
  if (!missing(increment_from)) object$yaxis$inc_from <- increment_from
  if (!missing(positions)) object$yaxis$pos <- position
  
  object
}


#' @rdname scplot
#' @param label Character string.
#' @param align Character string. One of "left", "right", "center"
#' @param wrap Number that defines the maximum characters per line before a break. 
#' If set to FALSE, no automatic linebreak is set.
#' @param parse If TRUE, the label is interpreted as an expression. Default = FALSE.
#' @export
add_title <- function(object, label, colour, size, font, align, parse) {
  
  if (!missing(colour)) object$theme$col.main <- colour
  if (!missing(size)) object$theme$cex.main <- size
  if (!missing(font)) object$theme$font.main <- font
  if (!missing(align)) object$theme$align.main <- align
  if (!missing(parse)) object$theme$parse.main <- parse
  
  object$title <- label
  object
}

#' @rdname scplot
#' @param label Character string.
#' @export
add_caption <- function(object, label, colour, size, font, align, wrap, margin, 
                        parse) {
  
  if (!missing(colour)) object$theme$col.caption <- colour
  if (!missing(size)) object$theme$cex.caption <- size
  if (!missing(font)) object$theme$font.caption <- font
  if (!missing(align)) object$theme$align.caption <- align
  if (!missing(wrap)) object$theme$wrap.caption <- wrap
  if (!missing(margin)) object$theme$margin.caption <- margin
  if (!missing(parse)) object$theme$parse.caption <- parse
  object$caption <- label
  object
}

#' @rdname scplot
#' @param positions Either a vector indicating the dot to be highlighted or a 
#' character string with a logical expression (e.g. values < mean(values))
#' @param shape Number. See pch graphical parameter on par help page.  
#' @export
add_marks <- function(object, case = 1, positions, 
                      colour = "red", size = 1, shape = 1, variable = ".dvar") {
  
  # Marks on the outliers from outlier()
  if (identical(class(positions), c("sc","outlier"))) {
    for(i in seq_along(positions$dropped.mt))
      object$marks <- c(
        object$marks, 
        list(
          list(
            case = i, positions = positions$dropped.mt[[i]], 
            col = colour, cex = size, pch = shape, variable = variable
          )
        )
      )
    return(object)
  }
  
  for(i in case) {
    object$marks <- c(
      object$marks, 
      list(
        list(
          case = i, positions = positions, col = colour, 
          cex = size, pch = shape, variable = variable
        )
      )
    )
  }
  
  
  object
}

#' @rdname scplot
#' @export
set_phasenames <- function(object, ..., colour, size) {
  
  if (!missing(colour)) object$theme$col.phasenames <- colour
  if (!missing(size)) object$theme$cex.phasenames <- size
  object$phase_names$labels <- c(...)
  object
}

#' @rdname scplot
#' @export
set_casenames <- function(object, ..., colour, side, size) {
  
  if (!missing(colour)) object$theme$col.casenames <- colour
  if (!missing(side)) object$theme$names$side <- side
  if (!missing(size)) object$theme$cex.casenames <- size
  
  
  object$case_names$labels <- c(...)
  object
}

#' @rdname scplot
#' @param x x position
#' @param y y position
#' @export
add_text <- function(object, case = 1, x, y, label, colour = NULL, size = NULL, 
                     angle = 0) {
  
  text <- list(case = case, labels = label, x = x, y = y, col = colour, cex = size, angle = angle)
  object$texts <- c(object$texts, list(text))
  object
}

#' @rdname scplot
#' @param case Numerical vector with the csae number or character string "all" for all cases.
#' @param x0 Origin x position of the line.
#' @param y0 Origin y position of the line.
#' @param x1 End x position of the line.
#' @param y1 End y position of the line.
#' @param length Size of the aroow angels.
#' @export
add_arrow <- function(object, case = 1, x0, y0, x1, y1, length = 0.1, 
                      colour = NULL, ...) {
  arrow <- list(
    case = case, 
    x0 = x0, 
    y0 = y0, 
    x1 = x1, 
    y1 = y1, 
    length = length, 
    col= colour,
    ...
  )
  object$arrows <- c(object$arrows, list(arrow))
  object
}

#' @rdname scplot
#' @export
add_grid <- function(object, type, width, colour) {

  object$theme$grid <- TRUE
  
  if (!missing(colour)) object$theme$col.grid <- colour
  if (!missing(width)) object$theme$lwd.grid <- width
  if (!missing(type)) object$theme$lty.grid <- type

  object
}

#' @rdname scplot
#' @export
set_background <- function(object, colour) {
  
  object$theme$fill.bg <- TRUE
  
  if (!missing(colour)) object$theme$col.fill.bg <- colour
  
  object
}

#' @rdname scplot
#' @export
add_frame <- function(object, colour = "black", size = 1, type = "solid") {
  
  object$theme$col.frame <- colour
  object$theme$lty.frame <- type
  object$theme$lwd.frame <- size
  
  object
}

#' @rdname scplot
#' @export
add_box <- function(object, colour = "black", type = "solid", width = 1) {
  
  object$theme$col.box <- colour
  object$theme$lty.box <- type
  object$theme$lwd.box <- width
  
  object
}

#' @rdname scplot
#' @param offset Number with shift from exact position of the label.
#' @param round Number of digits of the labels.
#' @param position Number 1, 2, 3,or 4, indicate positions below, to the left of, above and to the right of the exact value coordinate. 
#' @export
add_labels <- function(object, 
                       colour = "black", 
                       size = 0.6, 
                       offset = 0.4, 
                       position = 3, 
                       round = 1) {
  
  object$theme$annotations <- list(
    col = colour, cex = size, offset = offset, round = round, pos = position
  )
  object
}

#' @rdname scplot
#' @export
set_line <- function(object, colour, width, type, variable) {
  
  if (missing(variable)) id <- 1 else id <- which(names(object$theme$data_line) == variable)

  if (!missing(colour)) object$theme$data_line[[id]]$col <- colour
  if (!missing(width)) object$theme$data_line[[id]]$lwd <- width
  if (!missing(type)) object$theme$data_line[[id]]$lty <- type
  
  object
}

#' @rdname scplot
#' @export
set_dots <- function(object, colour, size, shape, variable) {
  
  
  if (missing(variable)) id <- 1 else id <- which(names(object$theme$data_dots) == variable)
  
  if (!missing(colour)) object$theme$data_dots[[id]]$col <- colour
  if (!missing(shape)) object$theme$data_dots[[id]]$pch <- shape
  if (!missing(size)) object$theme$data_dots[[id]]$cex <- size

  object
}

#' @rdname scplot
#' @export
add_ridge <- function(object, colour = "grey98") {
  
  object$theme$col.ridge <- colour
  object
}

#' @rdname scplot
#' @param extent A number between 0 and 1 given the proportion of the plot that is covert by the line or character string "full" or "scale".
#' @export
set_seperator <- function(object, colour, width, type, extent, label, size) {
  
  if (!missing(size)) object$theme$size.seperators <- size
  if (!missing(label)) object$label.seperators <- label
  if (!missing(colour)) object$theme$col.seperators <- colour
  if (!missing(width)) object$theme$lwd.seperators <- width
  if (!missing(type)) object$theme$lty.seperators <- type
  if (!missing(extent)) object$theme$extent.seperators <- extent
  
  object
}

#' @rdname scplot
#' @export
add_dataline <- function(object, variable, colour = NULL, width = NULL, 
                         type = NULL, colour_dots = NULL,
                         shape = NULL, size = NULL) {
  
  object$dvar <- c(object$dvar, variable)
  
  line <- list(list(col = colour, lwd = width, lty = type))
  names(line) <- variable
  
  dots <- list(list(col = colour, pch = shape, cex = size))
  names(dots) <- variable
  
  id <- which(names(object$theme$data_line) == ".default")
  if (length(id) == 0) object$theme$data_line <- c(object$theme$data_line, line)
  if (length(id) > 0) {
    names(object$theme$data_line)[id[1]] <- variable 
  }  
  
  id <- which(names(object$theme$data_dots) == ".default")
  if (length(id) == 0) object$theme$data_dots <- c(object$theme$data_dots, dots)
  if (length(id) > 0) {
    names(object$theme$data_dots)[id[1]] <- variable 
  }  
  
  object
}

#' @rdname scplot
#' @export
add_legend <- function(object, labels = ".default", x, y) {
  
  object$legend <- labels
  if (!missing(x)) object$theme$legend$x <- x
  if (!missing(y)) object$theme$legend$y <- y
  
  
  object
}

#' @rdname scplot
#' @param outer Vector with four values for the extension of the outer margins
#' (negative numbers for smaller margins): c(bottom, left, top, right).
#' @param outer Vector with four values for the extension of the inner margins
#' (negative numbers for smaller margins): c(bottom, left, top, right)
#' @export
add_margins <- function(object, outer, inner) {
  
  if (!missing(outer)) object$theme$oma <- object$theme$oma + outer
  if (!missing(inner)) object$theme$mar <- object$theme$mar + inner
  
  
  object
}


.check_theme <- function(theme) {
  
  if (!theme$ylab.orientation %in% 0:1) stop("wrong values for ylabel orientation")
  
  if (!is.null(theme$annotations)) {
    if(is.null(theme$annotations$round)) theme$annotations$round <- 1
    if(is.null(theme$annotations$offset)) theme$annotations$offset <- 0.5
  }
  
  theme
}
