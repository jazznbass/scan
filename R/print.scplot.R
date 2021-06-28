#' @export
print.scplot <- function(object) {
  
  data_list <- object$scdf
  dvar <- object$dvar
  pvar <- object$pvar
  mvar <- object$mvar
  ylim <- object$yaxis$lim
  xlim <- object$xaxis$lim
  style <- object$style
  
  style <- .check_style(style)
  
  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  
  par("oma" = c(4, 0, 2, 0))
  
  N <- length(data_list)
  
  if (N > 1) par(mfrow = c(N, 1))
  
  if (!is.null(style$col.frame)) style$bty <- "n"
  
  par("bg"       = style$col.bg)
  par("col"      = style$col)
  par("family"   = style$font)
  par("cex"      = style$cex)
  #par("las"      = style$las)
  #par("bty"      = style$bty)

  # prepare lines definitions
  object$lines <- .prepare_arg_lines(object$lines)
  
  # set global xlim and ylim ---------
  
  if (is.null(ylim)) {
    .dv <- unlist(lapply(data_list, function(x) x[, dvar]))
    ylim <- c(min(.dv, na.rm = TRUE), max(.dv, na.rm = TRUE))
  }
  if (is.null(xlim)) {
    .mt     <- unlist(lapply(data_list, function(x) x[, mvar]))
    xlim <- c(min(.mt, na.rm = TRUE), max(.mt, na.rm = TRUE))
  }
  
  # set margins --------
  
  w_line <- par("csi")
  
  w_axis <- strwidth(
    as.character(ylim[2]), 
    units = "inches", 
    cex = style$cex / par("cex") 
  )
  
  # Horizontal
  if (style$ylab.orientation == 1) {
    w_label <- strwidth(
      object$ylabel, 
      units = "inches", 
      cex = style$cex.ylab / par("cex")
    )
  }
  
  # Vertical
  if (style$ylab.orientation == 0) {
    w_label <- strheight(
      object$ylabel, 
      units = "inches", 
      cex = style$cex.ylab / par("cex")
    )
  }
  
  style$mai[2] <- w_axis + w_label + w_line * 2

  # Plotting cases ----------------------------------------------------------
  
  #par(mgp = c(2, 1, 0))
  
  for(case in 1:N) {
    data <- data_list[[case]]
    design <- .phasestructure(data, pvar)
    
    # plot ylim
    y_lim <- ylim
    if (is.na(ylim[2])) y_lim[2] <- max(data[, dvar])
    if (is.na(ylim[1])) y_lim[1] <- min(data[, dvar])
    
    # one plot
    
    if (N == 1) {
      add_topmar <- 0
      #add_topmar <- 2.5 * strheight(
      #  object$title, units = "inches", cex = style$cex.main / par("cex")
      #)
      par(mai = c(style$mai[1:2], style$mai[3] + add_topmar, style$mai[4]))
    } 
    
    # multple plots, first to secondlast
    if (N > 1 && case != N) {
      if (case == 1) { # first plot
        par(
          mai = c(
            style$mai[1] / 3, 
            style$mai[2], 
            style$mai[3] * 3, 
            style$mai[4]
          )
        ) 
      } else { # middle plot
        par(
          mai = c(
            style$mai[1] * 2 / 3, 
            style$mai[2], 
            style$mai[3] * 2, 
            style$mai[4]
          )
        ) 
      }
      
    }
    
    # multiple plots, last plot
    if (N > 1 && case == N) {
      par(mai = style$mai)
    } 
    
    plot(
      data[[mvar]], data[[dvar]], type = "n", 
      xlim = xlim, ylim = y_lim, ann = FALSE,
      xaxp = c(xlim[1], xlim[2], xlim[2] - xlim[1]),
      yaxt = "n", xaxt = "n", bty = "n"
    )
    
    # add xlab --------------------------------------------------------
    
    if (N == 1 || case == N) {
      mtext(
        object$xlabel, 
        side = 1, 
        line = 2, 
        las = 0, 
        cex = style$cex.xlab, 
        col = style$col.xlab
      )
    }
    
    # add ylab -------------------------------------------------------

    if (style$ylab.orientation == 0) 
      mtext(
        text = object$ylabel, side = 2, line = 2, las = 0, cex = style$cex.ylab, 
        col = style$col.ylab
      )
    
    if (style$ylab.orientation == 1) {
      mtext(
        text = object$ylabel, side = 2, line = 2, las = 1, at = max(y_lim), 
        cex = style$cex.ylab, col = style$col.ylab
      )
    }
    
    
    usr <- par("usr")
    
    # add background ----------------------------------------------------------
    
    if (isTRUE(style$fill.bg)){

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
    
    # add grid --------------------------------------------

    if (isTRUE(style$grid)) 
      grid(
        NULL, NULL, 
        col = style$col.grid, 
        lty = style$lty.grid, 
        lwd = style$lwd.grid
      )
    
    # add fill array below lines --------------------------------
    
    if (!is.null(style$col.ridge)) {
      for(i in 1:length(design$values)) {
        x <- data[design$start[i]:design$stop[i], mvar]
        y <- data[design$start[i]:design$stop[i], dvar]
        
        for(i in 1:length(x)) {
          x_values <- c(x[i], x[i + 1], x[i + 1], x[i])
          #y_values <- c(y_lim[1], y_lim[1], y[i + 1], y[i])
          y_values <- c(par("usr")[3], par("usr")[3], y[i + 1], y[i])
          polygon(x_values, y_values, col = style$col.ridge, border = NA)      
        }
      }
    }
    
    # add frame ---------------------------------
    
    if (!is.null(style$col.frame))
      rect(usr[1],usr[3],usr[2],usr[4], col = NA, border = style$col.frame)
    
    # add xaxis --------------------------------------------------------
    
    if (N == 1 || case == N) { #only when one plot or last plot
      xticks_pos <- seq(xlim[1], xlim[2], 1)
      axis(
        side = 1, 
        at = xticks_pos, 
        labels = FALSE, 
        col.axis = style$col.xaxis, 
        col.ticks = style$col.xaxis
      )
      text(
        x = seq(xlim[1], xlim[2], object$xaxis$inc),  
        y = par("usr")[3], 
        cex = style$cex.xaxis, 
        col = style$col.xaxis,
        labels = seq(xlim[1], xlim[2], object$xaxis$inc), 
        srt = 0, 
        pos = 1, 
        offset = style$cex.xaxis, 
        xpd = TRUE
      )
    }
    
    # add yaxis -------------------------------------------------------
    
    yticks_pos <- axTicks(2, usr = c(y_lim[1], ylim[2]))
    axis(
      side = 2, 
      at = yticks_pos, 
      labels = NA, 
      col.axis = style$col.yaxis, 
      col.ticks = style$col.yaxis
    )
    text(
      x = par("usr")[1], 
      y = yticks_pos, 
      labels = yticks_pos, 
      offset = style$cex.yaxis,
      col = style$col.yaxis,
      srt = 0, 
      pos = 2, 
      cex = style$cex.yaxis, 
      xpd = TRUE
    )
    
    # draw dv lines ---------------------------------------------
    for(i in 1:length(design$values)) {
      x <- data[design$start[i]:design$stop[i], mvar]
      y <- data[design$start[i]:design$stop[i], dvar]
      if (!is.null(style$col.lines)) {
        lines(
          x, y, 
          type = "l", 
          lty = style$lty, 
          lwd = style$lwd,
          col = style$col.lines
        )
      }
      if (!is.null(style$col.dots)) {
        points(
          x, y,
          pch = style$pch, 
          cex = style$cex.dots, 
          col = style$col.dots
        )
      }
    }
  
    # add title ----------------------------------------------------------
    
    if (FALSE) #case == 1) 
      title(
        main = object$title, 
        col.main = style$col.main, 
        cex.main = style$cex.main,
        font.main = style$font.main
      )
    
    # add marks ---------------------------------------------------------------
    
    if (!is.null(object$marks)) {
      
      id_case <- which(sapply(object$marks, function(x) x$case) == case)
      id_case <- c(
        id_case, 
        which(sapply(object$marks, function(x) x$case) == "all")
      )
      
      if (length(id_case) > 0) {
        for (i_marks in id_case) { 
          marks_case <- object$marks[[i_marks]]
          
          if (is.character(marks_case$positions)) {
            marks_case$positions <- eval(
              str2expression(marks_case$positions), envir = data
            )
            marks_case$positions <- which(marks_case$positions)
          }
          
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
    
    # add annotations ---------------------------------------------------------
    
    if (!is.null(style$annotations)) {
      text(
        x = data[,mvar], 
        y = data[,dvar], 
        label = round(data[, dvar], style$annotations$round), 
        col = style$annotations$col, 
        pos = style$annotations$pos, 
        offset = style$annotations$offset, 
        cex = style$annotations$cex
      )
    }
    
    # add lines ---------------------------------------------------------------
    
    if (!is.null(object$lines)) 
      .add_lines(data, mvar, dvar, pvar, design, object$lines)
  
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
    
    # add line between phases -------------------------------------------
    if (is.null(style$text.ABlag)) {
      for(i in 1:(length(design$values) - 1)) {
        x <- data[design$stop[i] + 1, mvar] - 0.5
        x <- c(x, x)
        
        if (style$seperators.extent == "full") y <- par("usr")[3:4]
        if (style$seperators.extent == "scale") y <- ylim
        if (is.numeric(style$seperators.extent)) {
          .range <- ylim[2] - ylim[1]
          .cut <- .range * (1 - style$seperators.extent) / 2
          y <- c(ylim[1] + .cut, ylim[2] - .cut)
        }
        
        lines(
          x, y, 
          lty = style$lty.seperators,
          lwd =  style$lwd.seperators, 
          col = style$col.seperators,
          xpd = TRUE
        )
        
        #abline(
        #  v = data[design$stop[i] + 1, mvar] - 0.5, 
        #  lty = style$lty.seperators, 
        #  lwd = style$lwd.seperators, 
        #  col = style$col.seperators
        #)
      }
    }
    
    if (!is.null(style$text.ABlag)) {
      for(i in 1:(length(design$values) - 1)) {
        tex <- paste(unlist(strsplit(style$text.ABlag[i], "")), collapse = "\n")
        text(
          x = data[design$stop[i] + 1, mvar] - 0.5, 
          y = (y_lim[2] - y_lim[1]) / 2 + y_lim[1], 
          labels = tex, 
          cex = 0.8)
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
          #args <- object$texts[[i]]
          #args <- args[-which(names(args) == "case")]
          #do.call(text, args)
          text(
            object$texts[[i]]$x, 
            object$texts[[i]]$y, 
            object$texts[[i]]$label, 
            srt = object$texts[[i]]$angle, 
            col = object$texts[[i]]$col, 
            cex = object$texts[[i]]$cex
          )
        }
        
      }
    }
    
    # add arrows --------------------------------------------------------
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
  
 

  # add title -----------------------------------------------------------
  
  if (!is.null(object$title)) {
    
    if (!is.null(style$wrap.title)) {
      object$title <- paste(
        strwrap(object$title, width = style$wrap.title),
        collapse = "\n"
      )
    }    
    
    adj <- NA
    
    if (style$align.main == "center") adj <- NA
    if (style$align.main == "left") adj <- 0 + style$margin.main
    if (style$align.main == "right") adj <- 1 - style$margin.main
    
    if (style$parse.main) object$title <- str2expression(object$title)
    
    mtext(
      object$title, 
      side = 3, 
      outer = TRUE, 
      cex = style$cex.main, 
      font = style$font.main, 
      col = style$col.main, 
      adj = adj 
    )
  }

  # add caption ---------------------------------------------------
  
  if (!is.null(object$caption)) {
    adj <- NA
    
    if (style$align.caption == "center") adj <- NA
    if (style$align.caption == "left") adj <- 0 + style$margin.caption
    if (style$align.caption == "right") adj <- 1 - style$margin.caption
    
    if (!is.null(style$wrap.caption)) {
      object$caption <- paste(
        strwrap(object$caption, width = style$wrap.caption),
        collapse = "\n"
      )
    }    
    
    if (style$parse.caption) object$caption <- str2expression(object$caption)
    
    mtext(
      object$caption, 
      side = 1, 
      line = 2, 
      outer = TRUE, 
      cex = style$cex.caption, 
      font = style$font.caption, 
      col = style$col.caption, 
      adj = adj
    )
  }  
  
  # add box ouround figure -----------------------------------------------

  par(op)
  
  if (!is.null(style$col.box)) {
    box(
      which = "figure", 
      lwd = style$lwd.box, 
      lty = style$lty.box, 
      col = style$col.box
    )
  }
  

  
}

.add_lines <- function(data, mvar, dvar, pvar, design, lines) {

    for(i_lines in seq_along(lines)) {
      
      line <- lines[[i_lines]]
      
      if (is.null(line[["lty"]])) line[["lty"]] <- "dashed"
      if (is.null(line[["lwd"]])) line[["lwd"]] <- 2
      if (is.null(line[["col"]])) line[["col"]] <- "black"
      
      lty.line <- line[["lty"]]
      lwd.line <- line[["lwd"]]
      col.line <- line[["col"]]
      
      if (line[["stat"]] == "trend") {
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
      if (line[["stat"]] == "median") {
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
      if (line[["stat"]] == "mean") {
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
      if (line[["stat"]] == "trendA") {
        x <- data[design$start[1]:design$stop[1], mvar]
        y <- data[design$start[1]:design$stop[1], dvar]
        maxMT <- max(data[, mvar])
        reg <- lm(y~x)
        lines(
          x = c(min(x), maxMT), 
          y = c(
            reg$coefficients[1] + min(x) * reg$coefficients[2], 
            reg$coefficients[1] + maxMT * reg$coefficients[2]
          ), 
          lty = lty.line, 
          col = col.line, 
          lwd = lwd.line
        )
      }
      if (line[["stat"]] == "trendA_bisplit") {
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
            reg$coefficients[1] + min(x) * reg$coefficients[2], 
            reg$coefficients[1] + maxMT * reg$coefficients[2]
          ), 
          lty = lty.line, 
          col = col.line, 
          lwd = lwd.line
        )
      }
      if (line[["stat"]] == "trendA_trisplit") {
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
      if (line[["stat"]] == "loreg") {
        if (is.null(line[["f"]])) line[["f"]] <- 0.5
        lines.par <- line[["f"]]
        reg <- lowess(data[,dvar] ~ data[,mvar], f = lines.par)
        lines(reg, lty = lty.line, col = col.line, lwd = lwd.line)
      }
      
      if (line[["stat"]] %in% c("maxA", "pnd")) {
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
      
      if (line[["stat"]] == "minA") {
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
      if (line[["stat"]] == "medianA") {
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
      if (line[["stat"]] == "meanA") {
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
      
      if (line[["stat"]] == "plm") {
        pr <- plm(data_list[case])
        y <- fitted(pr$full.model)
        lines(data[[mvar]], y, lty = lty.line, col = col.line, lwd = lwd.line)
      }
      if (line[["stat"]] == "plm.ar") {
        if (is.null(line[["ar"]])) line[["ar"]] <-2
        lines.par <- line[["ar"]]
        pr <- plm(data_list[case], AR = lines.par)
        y <- fitted(pr$full.model)
        lines(data[[mvar]], y, lty = lty.line, col = col.line, lwd = lwd.line)
      }
      
      if (line[["stat"]] == "movingMean") {
        if (is.null(line[["lag"]])) line[["lag"]] <- 1
        lines.par <- line[["lag"]]
        y <- .moving_average(data[, dvar],lines.par, mean)
        lines(data[, mvar], y, lty = lty.line, col = col.line, lwd = lwd.line)
      }
      if (line[["stat"]] == "movingMedian") {
        if (is.null(line[["lag"]])) line[["lag"]] <- 1
        lines.par <- line[["lag"]]
        y <- .moving_average(data[, dvar],lines.par, median)
        lines(data[, mvar], y, lty = lty.line, col = col.line, lwd = lwd.line)
      }
    }
  
}

