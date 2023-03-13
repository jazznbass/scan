errors <- c()

if (!requireNamespace("scplot", quietly = TRUE)) {
  errors <- c(errors, paste0("- You need to install the 'scplot' package to ",
  "run this app with devtools::install_github('jazznbass/scplot')\n"))
} 

if (!requireNamespace("shinyjs", quietly = TRUE)) {
  errors <- c(errors, paste0("- You need to install the 'shinyjs' package ",
    "to run this app with install.packages('shinyjs')\n"))
}

if (!requireNamespace("htmltools", quietly = TRUE)) {
  errors <- c(errors, paste0("- You need to install the 'htmltools' package ",
        "to run this app with install.packages('htmltools')\n"))
}

if (!requireNamespace("markdown", quietly = TRUE)) {
  errors <- c(errors, paste0("- You need to install the 'markdown' package ",
      "to run this app with install.packages('markdown')\n"))
}

if (length(errors) > 0) stop(errors)

suppressPackageStartupMessages({
library(shinyjs)
library(htmltools)
library(markdown)
library(scplot)
library(shiny)
})

res <- list()

res$choices <- list()
examples <- data(package = "scan")$results[, 3]
examples <- examples[!startsWith(examples,"Grosche2014")]
examples <- examples[!endsWith(examples,"Leidig2018_l2")]
examples <- examples[!startsWith(examples,"exampleAB_50.l2")]
res$choices$examples <- c(
  "(none)", 
  examples#substr(examples, 0, nchar(examples) - 12)
)

res$choices$scplot_examples <- c(
"(empty selection)" = "",

"Trend lines" =
'add_statline("trend")',

"Baseline trend" =
'add_statline("trendA")',

"Max A" = 'add_statline("max", phase = "A")',

"Means" = 'add_statline("mean")',
"Medians" = 'add_statline("median")',

"Moving average" = 'add_statline("movingMean")',

"Smoothed line" = 'add_statline("loess", span = 0.4)'
)

res$choices$scplot_templates_design <- c(
  "",
  "Legend" = 'add_legend()',
  "Add title" = 'add_title("A new plot", color = "darkblue", size = 1.3)',
  "Add caption" = 'add_caption("Note. What a nice plot!", face = "italic", colour = "darkred")',
  "Set axis labels" = 'set_ylabel("Score", color = "darkred", angle = 0)
set_xlabel("Session", color = "darkred")',
  "Set phase names" = 'set_phasenames(labels = c("Baseline", "Intervention", "Extended", "Follow-up"), color = "darkblue", size = 0.9, face = "italic")',
  "Set case names" = 'set_casenames(position = "strip", background = list(fill = "lightblue"))',
  "Resize size" = 'set_base_text(size = 19)',
  "Background" = 'set_background(fill = "grey90", color = "black", size = 2)'

)

themes <- names(scplot:::.scplot_themes)

for(i in seq_along(themes)) {
  res$choices$scplot_templates_design[[paste0("Theme ", themes[i])]] <-
    paste0('add_theme("', themes[i], '")')
}

res$choices$fn_stats <- c(
  "Descriptives" = "describe",
  "Standardized mean differences" = "smd",
  "Overlap indices" = "overlap",
  "Data trends" = "trend",
  "Auto correlations" = "autocorr",
  "Piecewise regression" = "plm",
  "Hierarchical piecewise regression" = "hplm",
  "Conservative Dual-Criterion" = "cdc",
  "Tau U" = "tau_u",
  "Baseline corrected tau" = "corrected_tau",
  "Percentage of non-overlapping data" =  "pnd",
  "Percent exceeding the median" = "pem",
  "Percent exceeding the trend" = "pet",
  "Percentage of all non-overlapping data" = "pand",
  "Nonoverlap of all Pairs" = "nap",
  "Randomization test" = "rand_test",
  "Outlier analysis" = "outlier"
)

res$choices$fn_plot <- c("scplot" = "scplot", "plot" = "plot.scdf")

res$placeholder$transform <- 'e.g.
values = scale(values)
values = local_regression(values)
values2 = values - max(values[phase=="A"])
across_cases(values2 = scale(values)
'

res$placeholder$plot_arguments <- 'for scplot:
choose one or more from the templates below and experiment with the syntax here.

e.g. for plot:
style = "sienna"
lines = list("loreg", f = 0.2, lty = "solid", col = "black", lwd = 3)
'

res$placeholder$mt <- "(optional, e.g. 1,2,4,6,7,8,9,12,13)"

res$placeholder$variables <-
"(optional, e.g., depression = 1,4,3,5,6,5,7
separate multiple variables with linebreaks)"

res$error_msg$invalid_case <- "Sorry!
The last case you tried to add didn't have a valid case definition."

res$error_msg$plot <- "Sorry!
The plot arguments are not valid."


res$msg$startup <-
"Welcome to 'shiny scan'!

You can:

1. create a new case (click 'Add')
2. load a dataset (click 'Load file' to import an rds, csv, or excel file)
3. choose an example scdf (choosse from 'Load example')

'exampleABC' is a good place to start.

The basic procedure is:

1. Choose or create an scdf (Single Case Data Frame) in the 'scdf' tab.
2. Optionally refine the scdf in the 'Transform' tab (e.g. select cases, recombine phases)
3. Analyse the data in the 'Stats' tab.
4. Create a plot in the 'Plot' tab.

Look at the 'Help' tab for more information.

Have fun!
"

res$msg$no_case_scdf <-
"No case has been defined yet.
You can:
1. create a new case (click 'Add')
2. load a dataset (click 'Load file' to import an rds, csv, or excel file)
3. choose an example scdf (choosse from 'Load example')
"

res$msg$no_case <-
"There is no case defined yet.
Please define a case on the 'scdf' tab first.
"

res$help_page <- "
#### Welcome to ***shiny scan***!

*Shiny-scan* is a graphical surface for *scan* (Single-Case Data Analysis). *scan* is an R package.

The basic procedure is:

1. Choose/ create a single case file in the **scdf tab**.
2. Optionally refine the case in the **Transform tab** (select cases, recombine phases, etc.)
3. Analyse the data in the **Stats tab**.
4. Create a plot in the **Plot tab**.

Analysis and plots are based on the scdf after any changes from the **Transform tab**.

You have two plot engines to choose from. scplot is much more powerful.

Here are helpful links:

[How to install shinyscan on your computer](https://github.com/jazznbass/shinyscan#readme)

[Help pages for scan](https://jazznbass.github.io/scan/)

[Online book for single case analysis with scan](https://jazznbass.github.io/scan-Book/)

[Help pages for scplot](https://jazznbass.github.io/scplot/)



Have fun!
"

# define js function for opening urls in new tab/window
res$java$window.open <- "
shinyjs.openURL = function(url) {
  window.open(url,'_blank');
}
"

### little help-functions

trim <- function(x) {
  gsub("\n", ", ", trimws(x))
  x <- gsub("\n", ", ", trimws(x))
  gsub(",,", ",", trimws(x))
}

quoted <- function(x) {
  out <- paste0("\"", x, "\"")
  out[is.na(x)] <- NA
  out
}

n2br <- function(x) gsub("\n", "<br>", x)
