# color styles ------------------------------------------------------------

.opt$style$default <- list(
  frame = "black",las = 1, mai = c(0.6, 0.58, 0.2, 0.2), 
  bty = "o", text.ABlag = NULL, pch = 17, font = "sans", 
  fill = FALSE, fill.bg = FALSE, grid = FALSE,  
  lwd = 2, lwd.seperators = 1.5, lwd.grid = 1,
  lty = "solid", lty.seperators = "dashed", lty.grid = "dotted",
  
  ylab.orientation = 0,
  
  cex = 1, cex.axis = 0.8, cex.text = 1, cex.lab = 1,
  cex.xlab = 1, cex.ylab = 1, cex.xaxis = 0.8, cex.yaxis = 0.8,
  cex.casenames = 1, cex.phasenames = 1, cex.dots = 0.8,
  
  col.lines = "black", col.dots = "black", col.seperators = "black", 
  col.fill = "grey75", col.fill.bg = "grey95", col.bg = "white", col = "black", 
  col.text = "black", col.casenames = "black", col.phasenames = "black",
  col.xlab = "black", col.ylab = "black", col.ridge = NULL,
  
  align.main = "center", align.caption = "left",
  margin.main = 0, margin.caption = 0.02, wrap.caption = NULL, wrap.main = NULL,
  parse.main = FALSE, parse.caption = FALSE,
  seperators.extent = "full",
  
  annotations = NULL, 
  
  names = list(side = 3, line = -1, adj = 0, col = "black", cex = 1)
)

.opt$style$yaxis <- list(
  ylab.orientation = 1, mai = c(0.6, 0.85, 0.2, 0.2), cex.lab = 0.8,
  cex.text = 0.8
)

.opt$style$tiny <- list(
  cex.text = 0.5, cex = 0.5, cex.lab = 0.5, 
  cex.casenames = 0.5, cex.phasenames = 0.5, cex.dots = 0.5,
  lwd = 0.7, lwd.seperators = 0.7, lwd.grid = 0.7,
  mai = c(0.3,0.3,0.1,0.05),
  names = list(side = 3, line = -1, adj = 0, col = "black", cex = 0.5)
)

.opt$style$small <- list(
  cex.text = 0.75, cex = 0.75, cex.lab = 0.75, cex.dots = 0.75,
  cex.casenames = 0.75, cex.phasenames = 0.75,
  lwd = 0.85, lwd.seperators = 0.85, lwd.grid = 0.85,
  mai = c(0.5,0.5,0.15,0.1),
  names = list(side = 3, line = -1, adj = 0, col = "black", cex = 0.75)
)

.opt$style$big <- list(
  cex.text = 1.25, cex = 1.25, cex.lab = 1.25, 
  cex.casenames = 1.25, cex.phasenames = 1.25, cex.dots = 1.25,
  lwd = 1.5, lwd.seperators = 1.5, lwd.grid = 1.5,
  mai = c(0.8,1,0.2,0.2),
  names = list(side = 3, line = -1, adj = 0, col = "black", cex = 0.75)
)


.opt$style$chart <- list(
  col.ridge = "grey50",
  fill.bg = TRUE, col.fill.bg = "grey98", fill = TRUE, col.fill = "grey50", 
  annotations = list(cex = 0.6, col = "black", offset = 0.4, round = 1, pos = 3),
  pch = 19, 
  frame = NA, grid = TRUE, col.grid = "grey75", lwd = 0.7, cex.text = 0.8, 
  cex.lab = 0.8, 
  cex.casenames = 0.8, cex.phasenames = 0.8
)


.opt$style$ridge <- list(
  col.ridge = "grey50",
  fill = "grey50", fill.bg = TRUE, col.fill.bg = "grey95", pch = 20
)

.opt$style$annotate <- list(
  annotations = list(cex = 0.6, col = "black", offset = 0.4, round = 1, pos = 3), pch = 19
)

.opt$style$grid <- list(
  frame = NA, grid = TRUE, col.grid = "lightblue", fill.bg = TRUE, 
  col.fill.bg = "grey95", lwd = 0.7, pch = 19, cex.axis = 0.8
)  

.opt$style$grid2 <- list(
  col.ridge = "white",
  fill = "white", grid = TRUE, col.grid = "lightgreen", frame = "black", 
  fill.bg = TRUE, col.fill.bg = "grey95", lwd = 0.7, pch = 1, cex.axis = 0.8
)  

.opt$style$dark <- list(
  fill.bg = TRUE, col.fill.bg = "black", bty = "o", col.lines = "gold", 
  col.bg = "grey10", col.dots = "red", col.seperators = "white", col = "white", 
  col.text = "white", col.casenames = "white", col.phasenames = "white")

.opt$style$nodots <- list(
  col.ridge = "grey95",
  type = "l", col.dots = "", fill = TRUE, col.fill = "grey95", grid = TRUE, 
  col.grid = "grey80", fill.bg = TRUE, col.fill.bg = "grey99")

.opt$style$sienna <- list(
  grid = TRUE, col.grid = "orange", pch = 18, col.lines = "grey85", 
  col.dots = "seagreen4", lwd = 2, col.bg = "seashell", fill.bg = "moccasin", 
  col.text = "sienna4", col = "darkolivegreen", col.seperators = "sienna4", 
  col.casenames = "sienna4", col.phasenames = "sienna4",
  cex.text = 0.8, cex.lab = 0.8, cex.axis = 0.7, 
  cex.casenames = 0.8, cex.phasenames = 0.8,
  frame = "darkseagreen", 
  font = "serif")

.opt$style$phase_color <- list(
  fill.bg = TRUE, col.fill.bg = c("aliceblue", "mistyrose1", "honeydew")
)

.opt$style$phase_shade <- list(
  fill.bg = TRUE,
  col.fill.bg = c("grey94", "grey99", "grey90")
)
