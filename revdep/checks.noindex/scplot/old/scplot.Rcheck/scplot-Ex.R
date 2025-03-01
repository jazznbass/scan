pkgname <- "scplot"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('scplot')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("add_arrow")
### * add_arrow

flush(stderr()); flush(stdout())

### Name: add_arrow
### Title: Add arrrows to an scplot
### Aliases: add_arrow

### ** Examples

data(exampleAB, package = "scan")
p1 <- scplot(exampleAB$Anja)  |>
  add_arrow(case = 1, 2, 70, 6, 55, color = "darkred")



cleanEx()
nameEx("add_grid")
### * add_grid

flush(stderr()); flush(stdout())

### Name: add_grid
### Title: Add grid to an scplot
### Aliases: add_grid

### ** Examples

data(exampleAB, package = "scan")
p1 <- scplot(exampleAB$Anja)  |>
  set_theme("minimal")  |>
  add_grid(color = "grey70")



cleanEx()
nameEx("add_legend")
### * add_legend

flush(stderr()); flush(stdout())

### Name: add_legend
### Title: Add a legend to an scplot
### Aliases: add_legend

### ** Examples

data(exampleAB_add, package = "scan")
scplot(exampleAB_add) |>
  set_dataline("depression") |>
  add_statline("mean") |>
  add_legend()

scplot(exampleAB_add) |>
  set_dataline(label = "Pychological Wellbeing") |>
  set_dataline("depression", color = "darkblue", label = "Depression") |>
  add_statline("mean", label = "Wellbeing mean") |>
  add_statline("mean", variable = "depression", label = "Depression mean") |>
  set_phasenames(color = NA) |>
  set_panel(fill = c("lightblue", "grey80")) |>
  add_legend(
    position = "left",
    section_labels = c("Variables", "Section"),
    title = list(color = "brown", size = 10, face = 2),
    text = list(color = "darkgreen", size = 10, face = 2),
    background = list(color = "lightgrey")
  )



cleanEx()
nameEx("add_line")
### * add_line

flush(stderr()); flush(stdout())

### Name: add_line
### Title: Add line to an scplot
### Aliases: add_line

### ** Examples

data(exampleAB, package = "scan")
p1 <- scplot(exampleAB$Anja)  |>
  add_line(hline = 70, color = "darkred") |>
  add_line(vline = 3, color = "blue") |>
  add_line(x0 = 1, y0 = 70, x1 = 4, y1 = 80, color = "green")



cleanEx()
nameEx("add_marks")
### * add_marks

flush(stderr()); flush(stdout())

### Name: add_marks
### Title: Add marks to an scplot
### Aliases: add_marks

### ** Examples

library(scan)
p1 <- scplot(exampleA1B1A2B2$Moritz) |> add_marks(positions = c(1,5,10,14))
p1 <- scplot(Huber2014) |> add_marks(positions = outlier(Huber2014))



cleanEx()
nameEx("new_theme")
### * new_theme

flush(stderr()); flush(stdout())

### Name: new_theme
### Title: Create a new scplot theme
### Aliases: new_theme extract_theme

### ** Examples

data(exampleABC, package = "scan")
my_theme <- new_theme() |>
  set_panel(color = "red")  |>
  set_base_text(size = 12, color = "blue")  |>
  set_dataline(color = "darkred", linewidth = 2)
p1 <- scplot(exampleABC)  |> set_theme(my_theme)



cleanEx()
nameEx("scplot.sc_hplm")
### * scplot.sc_hplm

flush(stderr()); flush(stdout())

### Name: scplot.sc_hplm
### Title: This function generates a forest plot for the random effects of
###   a mixed hplm model
### Aliases: scplot.sc_hplm

### ** Examples

model <- scan::hplm(scan::Leidig2018, random.slopes = TRUE)
scplot(model, effect = "level")




cleanEx()
nameEx("scplot.sc_rand")
### * scplot.sc_rand

flush(stderr()); flush(stdout())

### Name: scplot.sc_rand
### Title: Plot Randomization Effects
### Aliases: scplot.sc_rand

### ** Examples

## Not run: 
##D res <- scan::rand_test(scan::exampleAB$Anja, limit = 1)
##D scplot(res, type = "hist")
##D 
##D scplot(res, type = "xy")
## End(Not run)



cleanEx()
nameEx("scplot.sc_tauu")
### * scplot.sc_tauu

flush(stderr()); flush(stdout())

### Name: scplot.sc_tauu
### Title: Plot Tau-U Effects
### Aliases: scplot.sc_tauu

### ** Examples

res <- scan::tau_u(scan::Leidig2018)
scplot(res, effect = 3)




cleanEx()
nameEx("set_background")
### * set_background

flush(stderr()); flush(stdout())

### Name: set_background
### Title: Set plot and panel background of an scplot
### Aliases: set_background set_panel

### ** Examples

data(exampleAB, package = "scan")
p1 <- scplot(exampleAB)  |>
 set_background(fill = "lightblue", colour = "darkblue", linewidth = 1.5) |>
 set_panel(fill = "deepskyblue", color = "darkblue", linewidth = 0.3)



cleanEx()
nameEx("set_dataline")
### * set_dataline

flush(stderr()); flush(stdout())

### Name: set_dataline
### Title: Set data lines of an scplot
### Aliases: set_dataline add_dataline

### ** Examples

data(exampleAB_add, package = "scan")
scplot(exampleAB_add)  |>
  set_dataline("depression", color = "darkblue")



cleanEx()
nameEx("set_theme_element")
### * set_theme_element

flush(stderr()); flush(stdout())

### Name: set_theme_element
### Title: Set a theme element
### Aliases: set_theme_element

### ** Examples

data(exampleABC, package = "scan")
p1 <- scplot(exampleABC)  |>
  set_theme_element(
    axis.ticks.length = unit(0, "points"),
    axis.line.y = element_line(color = "darkred", linewidth = 2),
    panel.background = element_rect(color = "darkblue", linewidth = 1),
    panel.spacing.y = unit(0, "points"),
    phasenames = element_text(color = "#00000000")
  )



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
