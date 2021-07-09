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
  
  default_line <- list(
    variable = NULL, 
    col = NULL, 
    width = NULL, 
    linetype = NULL,
    dots = NULL,
    shape = NULL,
    size = NULL
  )
  
  out <- list(
    scdf = data,
    dvar = scdf_attr(data, .opt$dv), 
    pvar = scdf_attr(data, .opt$phase), 
    mvar = scdf_attr(data, .opt$mt),
    datalines = list(default_line),
    statlines = NULL,
    theme = theme,
    title = NULL,
    caption = NULL,
    xaxis = list(lim = NULL, inc = 1),
    yaxis = list(lim = NULL),
    xlabel = NULL,
    ylabel = NULL,
    statlines = NULL, 
    marks = NULL, 
    texts = NULL,
    arrows = NULL,
    phasenames = list(labels = ".default"),
    legend = NULL,
    casenames = list(labels = .case_names(names(data), length(data)))
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
#' @param color A character string or a number defining the color of an element.
#' @param size A number deifning the size of an element.
#' @param type A character string with the line type: "solid", "dashed", "dotted"
#' @export
add_statline <- function(object, stat, color, 
                         width, type, variable) {
  
  if (missing(color)) color <- object$theme$statline.col
  if (missing(width)) width <- object$theme$statline.width
  if (missing(type)) type <- object$theme$statline.linetype
  if (missing(variable)) variable <- ".dvar"
  
  line <- list(
    stat = stat, col = color, width = width, linetype = type, variable = variable
  )
  
  object$statlines <- c(object$statlines, list(line))
  object
  
}

#' @rdname scplot
#' @export
set_xlabel <- function(object, label, color, size) {
  
  if (!missing(color)) object$theme$xaxis.title.col <- color
  if (!missing(size)) object$theme$xaxis.title.size <- size
  if (!missing(label)) object$xlabel <- label

  object
}

#' @rdname scplot
#' @param orientation of the label: 0 = vertical; 1 = horizontal
#' @export
set_ylabel <- function(object, label, color, size, vjust, orientation) {
  
  if (!missing(orientation)) {
    if (orientation %in% c("h", "horizontal")) orientation <- 1
    if (orientation %in% c("v", "vertical")) orientation <- 0
    object$theme$yaxis.title.angle <- orientation
  }
  
  if (!missing(color)) object$theme$yaxis.title.col <- color
  if (!missing(size)) object$theme$yaxis.title.size <- size
  if (!missing(vjust)) object$theme$yaxis.title.vjust <- vjust
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
                      color, size, line, positions) {
  
  if (!missing(color)) object$theme$xaxis.text.col <- color
  if (!missing(size)) object$theme$xaxis.text.size <- size
  if (!missing(line)) object$theme$vjust.xlab <- line
  
  if (!missing(limits)) object$xaxis$lim <- limits
  if (!missing(increment)) object$xaxis$inc <- increment
  if (!missing(increment_from)) object$xaxis$inc_from <- increment_from
  if (!missing(positions)) object$xaxis$pos <- positions
  
  object
}

#' @rdname scplot
#' @export
set_yaxis <- function(object, limits, color, size, 
                      increment, increment_from, positions) {
  
  if (!missing(color)) object$theme$yaxis.text.col <- color
  if (!missing(size)) object$theme$yaxis.text.col <- size

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
add_title <- function(object, label, color, size, face, align, parse) {
  
  if (!missing(color)) object$theme$title.col <- color
  if (!missing(size)) object$theme$title.size <- size
  if (!missing(face)) object$theme$title.face <- face
  if (!missing(align)) object$theme$title.hjust <- align
  if (!missing(parse)) object$theme$title.parse <- parse
  
  object$title <- label
  object
}

#' @rdname scplot
#' @param label Character string.
#' @export
add_caption <- function(object, label, color, size, face, align, wrap, margin, 
                        parse) {
  
  if (!missing(color)) object$theme$caption.col <- color
  if (!missing(size)) object$theme$caption.size <- size
  if (!missing(face)) object$theme$caption.face <- face
  if (!missing(align)) object$theme$caption.align <- align
  if (!missing(wrap)) object$theme$caption.wrap <- wrap
  if (!missing(margin)) object$theme$caption.margin <- margin
  if (!missing(parse)) object$theme$caption.parse <- parse
  object$caption <- label
  object
}

#' @rdname scplot
#' @param positions Either a vector indicating the dot to be highlighted or a 
#' character string with a logical expression (e.g. values < mean(values))
#' @param shape Number. See pch graphical parameter on par help page.  
#' @export
add_marks <- function(object, case = 1, positions, 
                      color = "red", size = 1, shape = 1, variable = ".dvar") {
  
  # Marks on the outliers from outlier()
  if (identical(class(positions), c("sc","outlier"))) {
    for(i in seq_along(positions$dropped.mt))
      object$marks <- c(
        object$marks, 
        list(
          list(
            case = i, positions = positions$dropped.mt[[i]], 
            col = color, cex = size, pch = shape, variable = variable
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
          case = i, positions = positions, col = color, 
          cex = size, pch = shape, variable = variable
        )
      )
    )
  }
  
  
  object
}

#' @rdname scplot
#' @export
set_phasenames <- function(object, ..., color, size, x, y, box, frame) {
  
  if (!missing(color)) object$theme$phasenames.col <- color
  if (!missing(size)) object$theme$phasenames.size <- size
  if (!missing(x)) object$theme$phasenames.position.x <- x
  if (!missing(y)) object$theme$phasenames.position.y <- y
  if (!missing(box)) object$theme$phasenames.box.fill <- box
  if (!missing(frame)) object$theme$phasenames.box.col <- frame
  
  labels <-  c(...)
  if (!is.null(labels)) object$phasenames$labels <- labels
  object
}

#' @rdname scplot
#' @export
set_casenames <- function(object, ..., x, y, color, size) {
  
  if (!missing(color)) object$theme$casenames.col <- color
  if (!missing(size)) object$theme$casenames.size <- size
  if (!missing(x)) object$theme$casenames.position.x <- x
  if (!missing(y)) object$theme$casenames.position.y <- y
  labels <-  c(...)
  if (!is.null(labels)) object$casenames$labels <- labels
  object
}

#' @rdname scplot
#' @param x x position
#' @param y y position
#' @export
add_text <- function(object, case = 1, x, y, label, color = NULL, size = NULL, 
                     angle = 0) {
  
  text <- list(case = case, labels = label, x = x, y = y, col = color, cex = size, angle = angle)
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
                      color = NULL, ...) {
  arrow <- list(
    case = case, 
    x0 = x0, 
    y0 = y0, 
    x1 = x1, 
    y1 = y1, 
    length = length, 
    col= color,
    ...
  )
  object$arrows <- c(object$arrows, list(arrow))
  object
}

#' @rdname scplot
#' @export
add_grid <- function(object, type, width, color) {

  object$theme$grid <- TRUE
  
  if (!missing(color)) object$theme$grid.col <- color
  if (!missing(width)) object$theme$grid.width <- width
  if (!missing(type)) object$theme$grid.linetype <- type

  object
}

#' @rdname scplot
#' @export
set_background <- function(object, fill, frame) {
  
  if (!missing(fill)) object$theme$plot.background.fill <- fill
  if (!missing(frame)) object$theme$plot.background.col <- frame  
  
  object
}

#' @rdname scplot
#' @export
set_panel <- function(object, fill, frame, width, linetype) {
  
  if (!missing(fill)) object$theme$panel.col <- fill
  if (!missing(frame)) object$theme$panel.frame.col <- frame
  if (!missing(width)) object$theme$panel.frame.width <- width
  if (!missing(linetype)) object$theme$panel.frame.linetype <- linetype
  object
}

#' @rdname scplot
#' @param round Number of digits of the labels.
#' @export
add_labels <- function(object, 
                       color, 
                       size, 
                       nudge_y,
                       nudge_x,
                       vjust,
                       hjust,
                       round ,
                       box,
                       frame) {
  
  if (!missing(color)) object$theme$labels.col <- color
  if (!missing(size)) object$theme$labels.size <- size
  if (!missing(vjust)) object$theme$labels.vjust <- vjust
  if (!missing(hjust)) object$theme$labels.hjust <- hjust
  if (!missing(nudge_y)) object$theme$labels.nudge_y <- nudge_y
  if (!missing(nudge_x)) object$theme$labels.nudge_x <- nudge_x
  if (!missing(round)) object$theme$labels.round <- round
  if (!missing(box)) object$theme$labels.box.fill <- box
  if (!missing(frame)) object$theme$labels.box.col <- frame
  
  object
}

#' @rdname scplot
#' @export
add_ridge <- function(object, color = "grey98") {
  
  object$theme$ridge.col <- color
  object
}

#' @rdname scplot
#' @param extent A number between 0 and 1 given the proportion of the plot that is covert by the line or character string "full" or "scale".
#' @export
set_seperator <- function(object, color, width, type, extent, label, size) {
  
  if (!missing(size)) object$theme$seperators.size <- size
  if (!missing(label)) object$seperators$label <- label
  if (!missing(color)) object$theme$seperators.col <- color
  if (!missing(width)) object$theme$seperators.width <- width
  if (!missing(type)) object$theme$seperators.linetype <- type
  if (!missing(extent)) object$theme$seperators.extent <- extent
  
  object
}

#' @rdname scplot
#' @export
add_dataline <- function(object, variable, color, width, linetype, dots, shape, size) {
  
  object$dvar <- c(object$dvar, variable)
  
  if (missing(color)) color <- object$theme$dataline.col
  if (missing(width)) width <- object$theme$dataline.width
  if (missing(linetype)) linetype <- object$theme$dataline.linetype
  if (missing(dots)) dots <- color
  if (missing(shape)) shape <- object$theme$datadots.shape
  if (missing(size)) size <- object$theme$datadots.size
  
  new_line <- list(
    variable = variable, 
    col = color, 
    width = width, 
    linetype = linetype,
    dots = dots,
    shape = shape,
    size = size
  )
  
  object$datalines <- c(object$datalines, list(new_line))
  
  object
}

#' @rdname scplot
#' @export
set_dataline <- function(object, variable, color, width, linetype, dots, shape, size) {
  
  if (missing(variable)) variable <- ".dvar"
  
  vars <- sapply(object$datalines, 
                 function(x) if(is.null(x$variable)) ".dvar" else x$variable
          )
  id <- which(vars == variable)
  
  if (length(id) != 1) stop("Wrong variable defintion.")
  
  if (!missing(color)) object$datalines[[id]]$col <- color
  if (!missing(width)) object$datalines[[id]]$width <- width
  if (!missing(linetype)) object$datalines[[id]]$linetype <- linetype
  if (!missing(dots)) object$datalines[[id]]$dots <- dots
  if (!missing(shape)) object$datalines[[id]]$shape <-shape
  if (!missing(size)) object$datalines[[id]]$size <- size
  
  object
}


#' @rdname scplot
#' @export
add_legend <- function(object, labels = ".default", case = 1, x, y, 
                       datalines = TRUE, statlines = TRUE, title = NULL) {
  
  object$legend$labels <- labels
  if (!missing(x)) object$theme$legend.position.x <- x
  if (!missing(y)) object$theme$legend.position.y <- y
  object$legend$statlines <- statlines
  object$legend$datalines <- datalines
  object$theme$legend.position.case <- case
  object$legend$title <- title
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
  
  if (!theme$yaxis.title.angle %in% 0:1) stop("wrong values for ylabel angle")
  
  theme
}


