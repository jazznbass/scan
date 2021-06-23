#' @export
print.scplot <- function(object, ...) {
  
  dots <- list(...)
  
  data_list <- object$scdf
  dvar <- object$dvar
  pvar <- object$pvar
  mvar <- object$mvar
  ylim <- object$yaxis$lim
  ylab <- object$yaxis$label
  xlim <- object$xaxis$lim
  xinc <- object$xaxis$increase
  xlab <- object$xaxis$label
  main <- object$title
  phase_names <- object$phase_names$labels
  case_names <- object$case_names$labels
  style <- object$style
  lines <- object$lines
  marks <- object$marks
  
  style <- .check_style(style)
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  N <- length(data_list)
  
  if (N > 1) par(mfrow = c(N, 1))
  
  annotations <- style$annotations
  
  if (is.na(style$frame)) style$bty <- "n"
  
  par("bg"       = style$col.bg)
  par("col"      = style$col)
  par("family"   = style$font)
  par("cex"      = style$cex)
  #par("cex.lab"  = style$cex.lab)
  par("cex.axis" = style$cex.axis)
  par("las"      = style$las)
  par("bty"      = style$bty)
  #par("col.lab"  = style$col.text)
  par("col.axis" = style$col.text)
  
  if (isTRUE(style$frame == ""))   style$frame <- NA
  if (isTRUE(style$grid == ""))    style$grid  <- FALSE
  if (isTRUE(style$fill.bg == "")) style$fill.bg  <- FALSE
  
  
  
  # prepare lines definitions
  lines <- .prepare_arg_lines(lines)
  
  # set xlim and ylim
  values.tmp <- unlist(lapply(data_list, function(x) x[, dvar]))
  mt.tmp     <- unlist(lapply(data_list, function(x) x[, mvar]))
  
  if (is.null(ylim))
    ylim <- c(min(values.tmp, na.rm = TRUE), max(values.tmp, na.rm = TRUE))
  if (is.null(xlim))
    xlim <- c(min(mt.tmp, na.rm = TRUE), max(mt.tmp, na.rm = TRUE))
  
  
  # Plotting cases ----------------------------------------------------------
  
  par(mgp = c(2, 1, 0))
  
  for(case in 1:N) {
    data <- data_list[[case]]
    
    design <- .phasestructure(data, pvar)
    
    # plot ylim
    y.lim <- ylim
    if (is.na(ylim[2])) y.lim[2] <- max(data[, dvar])
    if (is.na(ylim[1])) y.lim[1] <- min(data[, dvar])
    
    # one plot
    if (N == 1) {
      if (main != "") par(mai = c(style$mai[1:2], style$mai[3] + 0.4, style$mai[4]))
      if (main == "") par(mai = style$mai)
      plot(
        data[[mvar]], data[[dvar]], type = "n", 
        xlim = xlim, ylim = y.lim, ann = FALSE,
        #xaxp = c(xlim[1], xlim[2], xlim[2] - xlim[1]), 
        yaxt = "n", 
        xaxt = "n", ...
      )
      
      xticks_pos <- seq(xlim[1], xlim[2], 1)
      axis(side = 1, at = xticks_pos, labels = FALSE)
      text(
        x = seq(xlim[1], xlim[2], xinc),  
        y = par("usr")[3], 
        cex = style$cex.axis, 
        labels = seq(xlim[1], xlim[2], xinc), 
        srt = 0, 
        pos = 1, 
        offset = style$cex.axis, 
        xpd = TRUE
      )
      
      yticks_pos <- axTicks(2, usr = c(y.lim[1], ylim[2]))
      axis(side = 2, at = yticks_pos, labels = NA)
      text(
        x = par("usr")[1], 
        y = yticks_pos, 
        labels = yticks_pos, 
        offset = style$cex.axis, 
        srt = 0, 
        pos = 2, 
        cex = style$cex.axis, 
        xpd = TRUE
      )
      
      if (style$ylab.orientation == 0) 
        mtext(text = ylab, side = 2, line = 2, las = 0, cex = style$cex.ylab, 
              col = style$col.ylab)
      
      if (style$ylab.orientation == 1) {
        mtext(
          text = ylab, side = 2, line = 2, las = 1, at = max(y.lim), 
          cex = style$cex.ylab, col = style$col.ylab
        )
      }
      
      mtext(text = xlab, side = 1, line = 2, las = 0, cex = style$cex.xlab, 
            col = style$col.xlab)
    } 
    
    # multple plots, first to secondlast
    if (N > 1 && case != N) {
      if (case == 1) { # first plot
        if (main != "") {
          par(mai = c(style$mai[1] * 2 / 6, style$mai[2], style$mai[3] * 3, style$mai[4]))
        }
        if (main == "") { 
          par(mai = c(style$mai[1] * 2 / 6, style$mai[2], style$mai[3] * 3, style$mai[4])) 
        }
      } else { # middle plot
        par(mai = c(style$mai[1] * 4 / 6, style$mai[2], style$mai[3] * 2, style$mai[4])) 
      }
      
      plot(
        data[[mvar]], data[[dvar]], xaxt = "n", yaxt = "n", type = "n", 
        xlim = xlim, ylim = y.lim, ann = FALSE, ...
      )
      
      yticks_pos <- axTicks(2, usr = c(y.lim[1], ylim[2]))
      axis(side = 2, at = yticks_pos, labels = NA)
      text(
        x = par("usr")[1], 
        y = yticks_pos, 
        labels = yticks_pos, 
        offset = style$cex.axis, 
        srt = 0, 
        pos = 2, 
        cex = style$cex.axis, 
        xpd = TRUE
      )
      
      if (style$ylab.orientation == 0) 
        mtext(ylab, side = 2, line = 2, las = 0, cex = style$cex.ylab, 
              col = style$col.ylab)
      
      if (style$ylab.orientation == 1)
        mtext(ylab, side = 2, line = 2, las = 1, at = max(y.lim), 
              cex = style$cex.ylab, col = style$col.ylab)
    }
    # multiple plots, last plot
    if (N > 1 && case == N) {
      par(mai = style$mai)
      plot(
        data[[mvar]], data[[dvar]], type = "n", 
        xlim = xlim, ylim = y.lim, ann = FALSE,
        xaxp = c(xlim[1], xlim[2], xlim[2] - xlim[1]),
        yaxt = "n", xaxt = "n",
        ...
      )
      
      xticks_pos <- seq(xlim[1], xlim[2], 1)
      axis(side = 1, at = xticks_pos, labels = FALSE)
      text(
        x = seq(xlim[1], xlim[2], xinc),  
        y = par("usr")[3], 
        cex = style$cex.axis, 
        labels = seq(xlim[1], xlim[2], xinc), 
        srt = 0, 
        pos = 1, 
        offset = style$cex.axis, 
        xpd = TRUE
      )
      
      yticks_pos <- axTicks(2, usr = c(y.lim[1], ylim[2]))
      axis(side = 2, at = yticks_pos, labels = NA)
      text(
        x = par("usr")[1], 
        y = yticks_pos, 
        labels = yticks_pos, 
        offset = style$cex.axis, 
        srt = 0, 
        pos = 2, 
        cex = style$cex.axis, xpd = TRUE
      )
      
      if (style$ylab.orientation == 0) 
        mtext(ylab, side = 2, line = 2, las = 0, cex = style$cex.ylab, 
              col = style$col.ylab)
      
      if (style$ylab.orientation == 1)
        mtext(ylab, side = 2, line = 2, las = 1, at = max(y.lim), 
              cex = style$cex.ylab, col = style$col.ylab)
      
      mtext(xlab, side = 1, line = 2, las = 0, cex = style$cex.xlab, 
            col = style$col.xlab)
      
    } 
    usr <- par("usr")
    
    # styling ------------------------------------------------------------
    
    # fill bg
    if(class(style$fill.bg) == "character") {
      style$col.fill.bg <- style$fill.bg
      style$fill.bg <- TRUE
    }
    
    if (isTRUE(style$fill.bg)){
      #rect(usr[1], usr[3], usr[2], usr[4], col = style$col.fill.bg, border = NA)
      
      type_phases <- unique(design$values)
      col <- rep(style$col.fill.bg, length = length(type_phases))
      
      for(i in seq_along(design$values)) {
        x <- data[design$start[i]:design$stop[i], mvar]
        y <- data[design$start[i]:design$stop[i], dvar]
        
        xmin <- min(x, na.rm = TRUE)
        xmax <- max(x, na.rm = TRUE) + 0.5
        if (i == length(design$values)) xmax <- usr[2]
        if (i > 1) xmin <- xmin - 0.5
        if (i == 1) xmin <- usr[1]
        .col <- col[which(type_phases == design$values[i])[1]]
        rect(xmin, usr[3], xmax, usr[4], col = .col, border = NA)
      }
    }
    
    # grid
    if(class(style$grid) == "character") {
      style$col.grid <- style$grid
      style$grid <- TRUE
    }
    
    if (isTRUE(style$grid)) 
      grid(NULL, NULL, col = style$col.grid, lty = style$lty.grid, lwd = style$lwd.grid)
    
    if (!is.na(style$frame))
      rect(usr[1],usr[3],usr[2],usr[4], col = NA, border = style$frame)
    
    if (is.na(style$frame) && isTRUE(style$fill.bg))
      rect(usr[1],usr[3],usr[2],usr[4], col = NA, border = style$col.fill.bg)
    
    if (is.na(style$frame) && !isTRUE(style$fill.bg))
      rect(usr[1],usr[3],usr[2],usr[4], col = NA, border = par("bg"))
    
    # fill array below lines
    
    if (style$fill == "" || is.na(style$fill)) style$fill <- FALSE
    
    if(class(style$fill) == "character") {
      style$col.fill <- style$fill
      style$fill <- TRUE
    }
    
    if (isTRUE(style$fill)) {
      for(i in 1:length(design$values)) {
        x <- data[design$start[i]:design$stop[i], mvar]
        y <- data[design$start[i]:design$stop[i], dvar]
        
        for(i in 1:length(x)) {
          x_values <- c(x[i], x[i+1], x[i+1], x[i])
          y_values <- c(y.lim[1], y.lim[1], y[i+1], y[i])
          polygon(x_values, y_values, col = style$col.fill, border = NA)      
        }
      }
    }
    
    # draw lines
    for(i in 1:length(design$values)) {
      x <- data[design$start[i]:design$stop[i], mvar]
      y <- data[design$start[i]:design$stop[i], dvar]
      if (style$col.lines != "") {
        lines(
          x, y, type = "l", pch = style$pch, lty = style$lty, lwd = style$lwd, 
          col = style$col.lines, ...
        )
      }
      if (style$col.dots != "") {
        lines(
          x, y, type = "p", pch = style$pch, lty = style$lty, lwd = style$lwd, 
          col = style$col.dots, ...
        )
      }
    }
    

    # add title ----------------------------------------------------------
    
    if (case == 1) 
      title(
        main = main, 
        col.main = style$col.main, 
        cex.main = style$cex.main,
        font.main = style$font.main
      )
    
    # add marks ---------------------------------------------------------------
    
    if (!is.null(marks)) {
      id_case <- which(sapply(object$marks, function(x) x$case) == case)
      if (length(id_case) > 0) {
        for (i_marks in id_case) { 
          marks_case <- marks[[i_marks]]
          marks.x <- data[data[, mvar] %in% marks_case$positions, mvar]
          marks.y <- data[data[, mvar] %in% marks_case$positions, dvar]
          points(
            x = marks.x, 
            y = marks.y,
            pch = marks_case$pch, 
            cex = marks_case$cex, 
            col = marks_case$col
          )
        }
      }
    }
    
    # annotations -------------------------------------------------------------
    
    if (!is.null(annotations)) {
      annotations.cex <- 1
      annotations.round <- 1
      annotations.col <- "black"
      annotations.pos <- 3
      annotations.offset <- 0.5
      
      if (any(names(annotations) == "cex")) {
        annotations.cex <- annotations[[which(names(annotations) == "cex")]]
      }
      if (any(names(annotations) == "col")) {
        annotations.col <- annotations[[which(names(annotations) == "col")]]
      }
      if (any(names(annotations) == "round")) {
        annotations.round <- annotations[[which(names(annotations) == "round")]]
      }
      if (any(names(annotations) == "pos")) {
        annotations.pos <- annotations[[which(names(annotations) == "pos")]]
      }
      if (any(names(annotations) == "offset")) {
        annotations.offset <- annotations[[which(names(annotations) == "offset")]]
      }
      
      annotations.label <- round(data[,dvar], annotations.round)
      ### not yet implemented
      #if (any(names(annotations) == "label")) {
      #  id <- which(names(annotations) == "label")
      #  if (annotations[[id]]=="values") {
      #  } else {
      #  }
      #}
      
      text(
        x = data[,mvar], 
        y = data[,dvar], 
        label = annotations.label, 
        col = annotations.col, 
        pos = annotations.pos, 
        offset = annotations.offset, 
        cex = annotations.cex, 
        ...
      )
    }
    
    # lines -------------------------------------------------------------------
    
    if (!is.null(lines)) {
      for(i_lines in seq_along(lines)) {
        
        line <- lines[[i_lines]]
        
        if (is.null(line[["lty"]])) line[["lty"]] <- "dashed"
        if (is.null(line[["lwd"]])) line[["lwd"]] <- 2
        if (is.null(line[["col"]])) line[["col"]] <- "black"
        
        lty.line <- line[["lty"]]
        lwd.line <- line[["lwd"]]
        col.line <- line[["col"]]
        
        if (line[["type"]] == "trend") {
          for(i in 1:length(design$values)) {
            x <- data[design$start[i]:design$stop[i], mvar]
            y <- data[design$start[i]:design$stop[i], dvar]
            reg <- lm(y~x)
            lines(
              x = c(min(x), max(x)), 
              y = c(
                reg$coefficients[1] + min(x) * reg$coefficients[2], 
                reg$coefficients[1] + max(x) * reg$coefficients[2]
              ), 
              lty = lty.line, 
              col = col.line, 
              lwd = lwd.line
            )
          }
        }
        if (line[["type"]] == "median") {
          for(i in 1:length(design$values)) {
            x <- data[design$start[i]:design$stop[i], mvar]
            y <- data[design$start[i]:design$stop[i], dvar]
            lines(
              x = c(min(x), max(x)), 
              y = c(median(y, na.rm = TRUE), median(y, na.rm = TRUE)), 
              lty = lty.line, 
              col = col.line, 
              lwd = lwd.line
            )
          }      
        }
        if (line[["type"]] == "mean") {
          if (is.null(line[["trim"]])) line[["trim"]] <- 0.1
          lines.par <- line[["trim"]]
          
          for(i in 1:length(design$values)) {
            x <- data[design$start[i]:design$stop[i],mvar]
            y <- data[design$start[i]:design$stop[i],dvar]
            lines(
              x = c(min(x), max(x)), 
              y = c(
                mean(y, trim = lines.par, na.rm = TRUE), 
                mean(y, trim = lines.par, na.rm = TRUE)
              ), 
              lty = lty.line, 
              col = col.line, 
              lwd = lwd.line
            )
          }
        }
        if (line[["type"]] == "trendA") {
          x <- data[design$start[1]:design$stop[1],mvar]
          y <- data[design$start[1]:design$stop[1],dvar]
          maxMT <- max(data[,mvar])
          reg <- lm(y~x)
          lines(
            x = c(min(x), maxMT), 
            y = c(
              reg$coefficients[1]  + min(x) * reg$coefficients[2], 
              reg$coefficients[1] + maxMT * reg$coefficients[2]
            ), 
            lty = lty.line, 
            col = col.line, 
            lwd = lwd.line
          )
        }
        if (line[["type"]] == "trendA_bisplit") {
          x     <- data[design$start[1]:design$stop[1],mvar]
          y     <- data[design$start[1]:design$stop[1],dvar]
          maxMT <- max(data[,mvar])
          # na.rm = FALSE for now to prevent misuse; 
          # will draw no line if NA present
          md1   <- c(median(y[1:floor(length(y)/2)], na.rm = FALSE),
                     median(x[1:floor(length(x)/2)], na.rm = FALSE))
          md2   <- c(median(y[ceiling(length(y)/2+1):length(y)], na.rm = FALSE),
                     median(x[ceiling(length(x)/2+1):length(x)], na.rm = FALSE))
          md    <- rbind(md1, md2)
          reg <- lm(md[,1]~md[,2])
          lines(
            x = c(min(x), maxMT), 
            y = c(
              reg$coefficients[1]  + min(x) * reg$coefficients[2], 
              reg$coefficients[1] + maxMT * reg$coefficients[2]
            ), 
            lty = lty.line, 
            col = col.line, 
            lwd = lwd.line
          )
        }
        if (line[["type"]] == "trendA_trisplit") {
          x     <- data[design$start[1]:design$stop[1],mvar]
          y     <- data[design$start[1]:design$stop[1],dvar]
          maxMT <- max(data[,mvar])
          # na.rm = FALSE for now to prevent misuse; 
          # will draw no line if NA present
          md1   <- c(
            median(y[1:floor(length(y) / 3)], na.rm = FALSE),
            median(x[1:floor(length(x) / 3)], na.rm = FALSE)
          )
          md2   <- c(
            median(y[ceiling(length(y) / 3 * 2 + 1):length(y)], na.rm = FALSE),
            median(x[ceiling(length(x) / 3 * 2 + 1):length(x)], na.rm = FALSE)
          )
          md    <- rbind(md1, md2)
          reg <- lm(md[,1] ~ md[,2])
          lines(
            x = c(min(x), maxMT), 
            y = c(
              reg$coefficients[1]  + min(x) * reg$coefficients[2], 
              reg$coefficients[1] + maxMT * reg$coefficients[2]
            ), 
            lty = lty.line, 
            col = col.line, 
            lwd = lwd.line
          )
        }
        if (line[["type"]] == "loreg") {
          if (is.null(line[["f"]])) line[["f"]] <- 0.5
          lines.par <- line[["f"]]
          reg <- lowess(data[,dvar] ~ data[,mvar], f = lines.par)
          lines(reg, lty = lty.line, col = col.line, lwd = lwd.line)
        }
        
        if (line[["type"]] %in% c("maxA", "pnd")) {
          x <- data[design$start[1]:design$stop[1],mvar]
          y <- data[design$start[1]:design$stop[1],dvar]
          maxMT <- max(data[,mvar])
          lines(
            x = c(min(x), maxMT), 
            y = c(max(y), max(y)), 
            lty = lty.line, 
            col = col.line, 
            lwd = lwd.line
          )		
        }
        
        if (line[["type"]] == "minA") {
          x <- data[design$start[1]:design$stop[1],mvar]
          y <- data[design$start[1]:design$stop[1],dvar]
          maxMT <- max(data[,mvar])
          lines(
            x = c(min(x), maxMT), 
            y = c(min(y), min(y)), 
            lty = lty.line, 
            col = col.line, 
            lwd = lwd.line
          )		
        }
        if (line[["type"]] == "medianA") {
          x <- data[design$start[1]:design$stop[1],mvar]
          y <- data[design$start[1]:design$stop[1],dvar]
          maxMT <- max(data[,mvar])
          
          lines(
            x = c(min(x), maxMT), 
            y = c(median(y, na.rm = TRUE), median(y, na.rm = TRUE)), 
            lty = lty.line, 
            col = col.line, 
            lwd = lwd.line
          )		
        }
        if (line[["type"]] == "meanA") {
          if (is.null(line[["trim"]])) line[["trim"]] <- 0.1
          lines.par <- line[["trim"]]
          
          x <- data[design$start[1]:design$stop[1],mvar]
          y <- data[design$start[1]:design$stop[1],dvar]
          maxMT <- max(data[,mvar])
          lines(
            x = c(min(x), maxMT), 
            y = c(
              mean(y, trim = lines.par, na.rm = TRUE), 
              mean(y, trim = lines.par, na.rm = TRUE)
            ), 
            lty = lty.line, 
            col = col.line, 
            lwd = lwd.line
          )		
        }
        
        if (line[["type"]] == "plm") {
          pr <- plm(data_list[case])
          y <- fitted(pr$full.model)
          lines(data[[mvar]], y, lty = lty.line, col = col.line, lwd = lwd.line)
        }
        if (line[["type"]] == "plm.ar") {
          if (is.null(line[["ar"]])) line[["ar"]] <-2
          lines.par <- line[["ar"]]
          pr <- plm(data_list[case], AR = lines.par)
          y <- fitted(pr$full.model)
          lines(data[[mvar]], y, lty = lty.line, col = col.line, lwd = lwd.line)
        }
        
        if (line[["type"]] == "movingMean") {
          if (is.null(line[["lag"]])) line[["lag"]] <- 1
          lines.par <- line[["lag"]]
          y <- .moving_average(data[, dvar],lines.par, mean)
          lines(data[, mvar], y, lty = lty.line, col = col.line, lwd = lwd.line)
        }
        if (line[["type"]] == "movingMedian") {
          if (is.null(line[["lag"]])) line[["lag"]] <- 1
          lines.par <- line[["lag"]]
          y <- .moving_average(data[, dvar],lines.par, median)
          lines(data[, mvar], y, lty = lty.line, col = col.line, lwd = lwd.line)
        }
      }
    }#### END: Adding help-lines
    
    
    # add phase names ---------------------------------------------------------
    if (is.null(object$phase_names$labels)) {
      case_phase_names <- design$values
    } else {
      case_phase_names <- object$phase_names$labels
    }
    
    
    for(i in 1:length(design$values)) {
      mtext(
        text = case_phase_names[i], 
        side = 3, 
        at = (data[design$stop[i], mvar] - data[design$start[i], mvar]) / 2 + 
          data[design$start[i], mvar], 
        cex = style$cex.phasenames,
        col = style$col.phasenames
      )
    }
    
    # line between phases -----------------------------------------------------
    if (is.null(style$text.ABlag)) {
      for(i in 1:(length(design$values) - 1)) {
        abline(
          v = data[design$stop[i] + 1, mvar] - 0.5, 
          lty = style$lty.seperators, 
          lwd = style$lwd.seperators, 
          col = style$col.seperators
        )
      }
    }
    
    if (!is.null(style$text.ABlag)) {
      for(i in 1:(length(design$values) - 1)) {
        tex <- paste(unlist(strsplit(style$text.ABlag[i], "")), collapse = "\n")
        text(
          x = data[design$stop[i] + 1, mvar] - 0.5, 
          y = (y.lim[2] - y.lim[1]) / 2 + y.lim[1], 
          labels = tex, 
          cex = 0.8, ...)
      }
      
    }
    
    # add case name -----------------------------------------------------
    if (!is.null(object$case_names$labels)) {
      args <- c(
        list(text = object$case_names$labels[case]), 
        cex = style$cex.casenames,
        col = style$col.casenames,
        style$names, 
        at = min(xlim)
      )
      args <- args[!duplicated(names(args))]
      do.call(mtext, args)
    }
    
    
    # add text ----------------------------------------------------------
    if (length(object$texts) > 0) {
      id_case <- which(sapply(object$texts, function(x) x$case) == case)
      if (length(id_case) > 0) {
        for(i in id_case) {
          args <- object$texts[[i]]
          args <- args[-which(names(args) == "case")]
          do.call(text, args)
          #text(text$x, text$y, text$label)
        }
        
      }
    }
    
    # adding arrows -------------------------------------------------------------
    if (length(object$arrows) > 0) {
      id <- which(sapply(object$arrows, function(x) x$case) == case)
      if (length(id) > 0) {
        for(i in id) {
          args <- object$arrows[[i]]
          args <- args[-which(names(args) == "case")]
          do.call(arrows, args)
        }
        
      }
    }
    
  }
  
}