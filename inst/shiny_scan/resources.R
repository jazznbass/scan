errors <- c()

if (length(errors) > 0) stop(errors)

suppressPackageStartupMessages({
library(scan)
library(scplot)
library(shiny)
library(bslib)
})

options(
  scan.export.kable_styling = list(
    bootstrap_options = c("striped", "condensed"), 
    full_width = FALSE,
    position = "left"
  ),
  scan.export.title.prefix = NULL
)

res <- list()

res$pipe <- " |> "
res$pipe_br <- paste0(res$pipe, "\n")

res$new_case <- "-new case-"

# choices ------

res$choices <- list()
examples <- data(package = "scan")$results[, 3]
filter <- startsWith(examples,"Grosche2014") | 
          endsWith(examples,"Leidig2018_l2") | 
          startsWith(examples,"exampleAB_50.l2")
examples <- examples[!filter]

res$choices$examples <- c("(none)", examples)

res$choices$scplot_examples <- c(
"(empty selection)" = "",
"Trend lines" = 'add_statline("trend")',
"Baseline trend" = 'add_statline("trendA")',
"Max A" = 'add_statline("max", phase = "A")',
"Means" = 'add_statline("mean")',
"Medians" = 'add_statline("median")',
"Moving average" = 'add_statline("moving mean")',
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

res$choices$scplot_templates_annotate <- c(
  "",
  "Marks" = 'add_marks(case = "all", position = \"values < mean(values)\", shape = 16, size = 2)',
  "Text" = 'add_text("Hallo", case = 1, x = 5, y = 20)',
  "Arrow" = 'add_arrow(case = 1, 2, 70, 6, 55, color = "darkred")'
)  

themes <- names(scplot:::.scplot_themes)

for(i in seq_along(themes)) {
  res$choices$scplot_templates_design[[paste0("Theme ", themes[i])]] <-
    paste0('set_theme("', themes[i], '")')
}

res$choices$fn_stats <- c(
  "Descriptives" = "describe",
  "Standardized mean differences" = "smd",
  "Between-Case Standardized Mean Difference" = "between_smd",
  "Overlap indices" = "overlap",
  "Data trends" = "trend",
  "Auto correlations" = "autocorr",
  "Piecewise regression" = "plm",
  "Multivariate piecewise regression" = "mplm",
  "Hierarchical piecewise regression" = "hplm",
  "Bayesian piecewise regression" = "bplm",
  "Conservative Dual-Criterion" = "cdc",
  "Tau U" = "tau_u",
  "Baseline corrected tau" = "corrected_tau",
  "Percentage of non-overlapping data" =  "pnd",
  "Percent exceeding the median" = "pem",
  "Percent exceeding the trend" = "pet",
  "Percentage of all non-overlapping data" = "pand",
  "Improvement rate difference" = "ird",
  "Nonoverlap of all Pairs" = "nap",
  "Randomization test" = "rand_test",
  "Outlier analysis" = "outlier"
)

.name <- function(x, at, label) {
  names(x)[which(x == at)] <- label
  x
}


res$choices$pt_method <- names(scan:::.opt$mc_fun)  |> 
  setNames(names(scan:::.opt$mc_fun)) |> 
  .name("plm_level", "Regression (level effect)") |> 
  .name("plm_slope", "Regression (slope effect)") |> 
  .name("plm_poisson_level", "Regression (level effect; frequencies)") |> 
  .name("plm_poisson_slope", "Regression (slope effect; frequencies)") |> 
  .name("hplm_level", "Multilevel-regression (level effect)") |> 
  .name("hplm_slope", "Multilevel-regression (slope effect)") |> 
  .name("tauU", "Tau-U (trend A)") |>
  .name("tauU_slope", "Tau-U (trend A and B)") |>
  .name("tauU_meta", "Metaanalysis Tau-U (trend A)") |>
  .name("tauU_slope_meta", "Metaanalysis Tau-U (trend A and B)") |>
  .name("base_tau", "Baseline corrected tau") |>
  .name("rand", "Randomization test") |> 
  .name("rand_decrease", "Randomization test (decreasing)") |> 
  .name("rand_slope", "Randomization test slope") |>
  .name("rand_slope_decrease", "Randomization test slope (decreasing)")




# placeholder ----

res$placeholder$values <- "Enter values here to create a new case. E.g. \nA = 1,2,3,4,3 \nB = 7,6,7,8,7,6"

res$placeholder$transform <- 'e.g.
values = scale(values)
values = local_regression(values)
values2 = values - max(values[phase=="A"])
across_cases(values2 = scale(values)
'

res$placeholder$stats_out_args <- "" #"e.g.: decimals = 3; meta = FALSE"

res$placeholder$plot_arguments <- '(choose one or more of the templates below and experiment with the syntax here.)
'

res$placeholder$mt <- "(optional, e.g. 1,2,4,6,7,8,9,12,13)"

res$placeholder$variables <-
"(optional, e.g., depression = 1,4,3,5,6,5,7
separate multiple variables with linebreaks)"

res$placeholder$pt <- "Power calculation may take some time. Click 'Run' to start calculation."

# div ------

res$div$settings <- paste0(
  "background-color:#f0f0f0; ",
  "border: 1px solid black; border-radius: 5px; ",
  "padding-left: 10px; padding-right: 10px; ",
  "padding-top: 0px; padding-bottom: 0px; "
)

res$div$pt <- paste0(
  "background-color:#f0f0f0; ",
  "border: 1px solid black; border-radius: 5px; ",
  "padding-left: 5px; padding-right: 5px; ",
  "padding-top: 0px; padding-bottom: 0px; "
)

# error ----

res$error_msg$invalid_case <- "Sorry!
The case you just tried to add didn't have a valid case definition."

res$error_msg$plot <- "Sorry!
The plot arguments are not valid."

res$error_msg$scdf_save <- "Sorry!
The last file you saved is corrupt. Did you forget to add a case before saving?"

res$error_msg$html_output <- "Sorry!
HTML export is not available. Please switch to text output instead."

# msg ----

res$msg$startup <-
"Welcome to 'shiny scan'!

You can:

1. load a dataset: click 'Choose file' to import an rds, csv, or excel file)
2. choose an example scdf from 'Choose example')
3. create a new case (Data -> New -> fill in 'values' and click 'save case')

'exampleABC' is a good place to start.

The basic procedure is:

1. Choose or create an scdf (Single Case Data Frame) in the 'Data' tab.
2. Optionally refine the scdf in the 'Transform' tab (e.g. select cases, recombine phases)
3. Analyse the data in the 'Stats' tab.
4. Create a plot in the 'Plot' tab.

Look at the 'Help' tab for more information.

Have fun!
"

res$msg$no_case_scdf <-
"No case has been defined yet.
You can:
1. create a new case (fill in 'values' and click 'save case')
2. load a dataset (Data -> Load -> click 'Open file' to import an rds, csv, or excel file)
3. choose an example scdf (Data -> Load -> 'Choose example')
"

res$msg$no_case <-
"There is no case defined yet.
Please define a case on the 'Data' tab first.
"

res$msg$load_page <- "Please choose an example scdf or open a file in rds, xlsx, csv, or R-code format."

res$help_page <- structure(
  "<h4 id=\"welcome-to-shiny-scan\">Welcome to <em><strong>shiny scan</strong></em>!</h4>
  <p><em>Shiny-scan</em> is a graphical surface for <em>scan</em> (Single-Case Data Analysis). <em>scan</em> is an R package.</p>
  <p>The basic procedure is:</p>\n<ol>
  <li>Choose/ create a single case file in the <strong>Data</strong> tab.</li>
  <li>Optionally refine the case in the <strong>Transform tab</strong> (select cases, recombine phases, etc.)</li>
  <li>Analyse the data in the <strong>Stats tab</strong>.</li>
  <li>Create a plot in the <strong>Plot tab</strong>.</li>\n</ol>
  <p>Analysis and plots are based on the scdf after any changes from the <strong>Transform tab</strong>.</p>
  <p>Here are helpful links:</p>
  <p><a href=\"https://jazznbass.github.io/scan-Book/ch_shinyscan.html\">A short introduction to shiny scan</a></p>
  <p><a href=\"https://jazznbass.github.io/scan-Book/\">Online book for single case analysis with scan</a></p>
  <p><a href=\"https://jazznbass.github.io/scan/\">Technnical help pages for scan</a></p>
  <p><a href=\"https://jazznbass.github.io/scplot/\">Technnical help pages for scplot</a></p>
  <p>Have fun!</p>",
  html = TRUE, class = c("html", "character")
)

## ---- define themes (global scope) ----
res$theme_light <- bs_theme(version = 5, bootswatch = "cerulean")

res$theme_dark <- res$theme_light |>
  bs_theme_update(
    # High-contrast dark surface + readable text
    bg = "#0f172a", fg = "#e5e7eb",
    # Cards/tables to match dark surface
    "card-bg"            = "#111827",
    "card-border-color"  = "#1f2937",
    "table-bg"           = "#0f172a",
    "table-border-color" = "#1f2937"
    # (Optional) tweak accent colors; by default we keep Cerulean's primary blue
    # "primary" = "#2FA4E7"
  )



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

correct_casenames <- function(x, string = FALSE) {
  
  if (is.null(x) || identical(length(x), 0)) return(NULL)
    
  new_names <- names(x)
  if (is.null(new_names)) {
    new_names <- paste0("[case #", 1:length(x), "]")
  } else {
    nonames <- which(is.na(new_names) | new_names == "")
    new_names[nonames] <- paste0("[case #", nonames, "]")
  }
  
  while(anyDuplicated(new_names) != 0) {
    id <- anyDuplicated(new_names) 
    new_names[id] <- paste0(new_names[id], "'")
  }
  
  if (string) {
    return(new_names)
  }
  
  names(x) <- new_names
  x
}

guess_col <- function(cols, hints) {
  hit <- which(tolower(cols) %in% hints)
  if (length(hit)) cols[hit[1]] else cols[1]
}

n2br <- function(x) gsub("\n", "<br>", x)

render_summary <- function(scdf) {
  out <-  summary(scdf) |> export()
  if (getOption("scan.export.engine") == "gt")
    out <- out |> gt::as_raw_html(out)
  HTML(out)
}
