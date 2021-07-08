
# scplot themes -----------------------------------------------------------


.opt$scplot_themes$default = list(
  
  mar = c(bottom = 0, left = 0, top = 1, right = 0.5),
  oma = c(0, 0, 0, 0),
  
  vjust.xlab = 0, vjust.ylab = 0,
  
  font = "sans", 
 
  
  ylab.orientation = 0,
  
  cex.xlab = 1, cex.ylab = 1, 

  panel.col = "white",
  panel.frame.col = NULL,
  panel.frame.width = 1,
  panel.frame.linetype = "solid",
  
  
  xaxis.text.col = "black",
  xaxis.text.size = 0.8,  
  xaxis.text.angel = 0,
  
  yaxis.text.col = "black",
  yaxis.text.size = 0.8,  
  yaxis.text.angel = 0,
  
  col.xlab = "black", col.ylab = "black", col.ridge = NULL,
  
  col.line.xaxis = "black", col.ticks.xaxis = "black",
  col.line.yaxis = "black", col.ticks.yaxis = "black",

  
  align.main = "center", align.caption = "left",
  margin.main = 0, margin.caption = 0.02, 
  wrap.caption = NULL, wrap.main = NULL,
  parse.main = FALSE, parse.caption = FALSE,
  length.ticks.xaxis = -0.3, length.ticks.yaxis = -0.3,
  
  plot.fill = NULL,
  plot.frame.col = NULL,
  plot.frame.width = 1, 
  plot.frame.linetype = "solid",
  
  labels.col = NULL,
  labels.size = 0.6,
  labels.round = 1,
  labels.vjust = 0.5,
  labels.hjust = 0.5,
  labels.nudge_x = 0,
  labels.nudge_y = 1.5, 
  labels.family = "sans",
  labels.face = 0,
  labels.box.fill = NULL,
  labels.box.col = NULL,
  labels.box.width = 1,
  labels.box.type = "solid",
  labels.box.margin = 0.2,

  phasenames.col = "black",
  phasenames.size = 1,
  phasenames.position.x = "centre",
  phasenames.position.y = "above",
  phasenames.family = "sans",
  phasenames.face = "plain",
  phasenames.box.fill = NULL,
  phasenames.box.col = NULL,
  phasenames.box.width = 1,
  phasenames.box.type = "solid",
  phasenames.box.margin = 0.2,
  
  casenames.col = "black",
  casenames.size = 1,
  casenames.position.x = NULL,
  casenames.position.y = NULL,
  
  grid.width = 1, 
  grid.linetype = "dotted", 
  grid.col = NULL,

  seperators.linetype = "dashed",
  seperators.width = 1.5,
  seperators.extent = "full",
  seperators.col = "black",
  
  legend.text.size = 0.8,
  legend.text.col = "black",
  legend.bg.col = "white",
  legend.position.case = 1,
  legend.position.x = NULL,
  legend.position.y = NULL,  
  legend.line.length = 1,
  
  statline.col = "grey",
  statline.width = 1,
  statline.linetype = "solid",
  
  dataline.col = "black",
  dataline.width = 2,
  dataline.linetype = "solid",
  
  datadots.col = "black",
  datadots.shape = 16,
  datadots.size = 0.8,
  
  NULL
)

.opt$scplot_themes$tiny <- list(
  
  cex.xlab = 0.5, cex.ylab = 0.5, 
  xaxis.text.size = 0.5, yaxis.text.size = 0.5,
  casenames.size = 0.5, 
  phasenames.size = 0.5, 
  grid.width = 0.7,
  dataline.width = 0.7,
  datadots.size = 0.5,
  seperators.width = 0.7
  
)

.opt$scplot_themes$small <- list(
  
  cex.xlab = 0.75, cex.ylab = 0.75,
  xaxis.text.size = 0.75, yaxis.text.size = 0.75,
  casenames.size = 0.75, phasenames.size = 0.75,
  grid.width = 0.85,
  dataline.width = 0.85,
  datadots.size = 0.75,  
  seperators.width = 0.85
)

.opt$scplot_themes$big <- list(
  cex.xlab = 1.25, cex.ylab = 1.25,
  xaxis.text.size = 1.25, yaxis.text.size = 1.25,
  casenames.size = 1.25, phasename.sizes = 1.25, 
  grid.width = 1.5,
  dataline.width = 1.5,
  datadots.size = 1.25,
  seperators.width = 1.5
)


.opt$scplot_themes$chart <- list(
  col.ridge = "grey50",
  panel.col = "grey98",
 
  grid.col = "grey75",
  cex.ylab = 0.8, cex.xlab = 0.8,
  casenames.size = 0.8, phasenames.size = 0.8,
  dataline.width = 0.7,
  labels.col = "black"

)

.opt$scplot_themes$grid <- list(
  grid.col = "lightblue", panel.col = "grey95", 
  lwd.line = 0.7, pch = 19, xaxis.text.size = 0.8, yaxis.text.size = 0.8,
  casenames.size = 0.8, phasenames.size = 0.8
)  

.opt$scplot_themes$grid2 <- list(
  col.ridge = "white", grid.col = "lightgreen", col.frame = "black", 
  panel.col = "grey95", 
  lwd.line = 0.7, pch = 1, xaxis.text.size = 0.8, yaxis.text.size = 0.8
)  

.opt$scplot_themes$dark <- list(
  panel.col = "#16213E", 
  plot.fill = "#1A1A2E", 
  grid.col = "#999999",
  casenames.col = "white", phasenames.col = "white",
  col.xlab = "white", col.ylab = "white",
  xaxis.text.col = "white", yaxis.text.col = "white",
  col.line.xaxis = "#DDDDDD", col.ticks.xaxis = "#DDDDDD",
  col.line.yaxis = "#DDDDDD", col.ticks.yaxis = "#DDDDDD",
  
  
  seperators.col = "gold",
  
  dataline.col = "#DDDDDD",
  dataline.width = 2,
  dataline.linetype = "solid",
  
  datadots.col = "#E94560",
  datadots.shape = 17,
  datadots.size = 0.8
)

.opt$scplot_themes$nodots <- list(
  col.ridge = "grey95",
  datadots.col = NULL,
  plot.fill = "grey95", grid.col = "grey80", panel.col = "grey99"
)

.opt$scplot_themes$sienna <- list(
  grid.col = "orange", 
  plot.fill = "seashell", 
  panel.col = "moccasin", col.frame = "darkseagreen", 
  
  casenames.col = "sienna4", phasenames.col = "sienna4",
  cex.ylab = 0.8, cex.xlab = 0.8, xaxis.text.size = 0.7, yaxis.text.size = 0.7,
  casenames.size = 0.8, phasenames.size = 0.8,
  
  seperators.col = "sienna4",
  
  
  dataline.col = "darkolivegreen",
  dataline.width = 2,
  dataline.linetype = "solid",
  
  datadots.col = "seagreen4",
  datadots.shape = 18,
  datadots.size = 0.8,

  font = "serif"
)

.opt$scplot_themes$phase_color <- list(
  panel.col = c("aliceblue", "mistyrose1", "honeydew")
)

.opt$scplot_themes$phase_shade <- list(
  panel.col = c("grey94", "grey99", "grey90")
)



