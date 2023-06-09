source("resources.R")

# scdf ------
tab_scdf <-   tabPanel(
  "Data",
  sidebarLayout(
    sidebarPanel(
      textAreaInput(
        "values", "Values", placeholder = res$placeholder$values
      ),
      textInput("mt", "Measurement times", placeholder = res$placeholder$mt),
      textAreaInput(
        "variables", "Additional variables",
        placeholder = res$placeholder$variables
      ),
      textInput("casename", "Case name", placeholder = "(optional)"),
      actionButton("add_case", "Add case"),
      actionButton("remove_case", "Remove case"),
      actionButton("remove_all", "Clear all cases"),
      br(),
      div(style="display:inline-block;",
          radioButtons(
            "remove_which", "Position", choices = c("last", "at"), inline = TRUE
          ),
      ),
      div(style="display:inline-block; padding-left: 30px;",
          numericInput("remove_at", "At", min = 1,value = 1),
      ),
      br(),
      selectInput(
        "scdf_example", "Load example", choices = res$choices$examples,
      ),
      fileInput(
        "upload", NULL, accept = c(".csv", ".rds", ".xlsx", ".xls", ".R", ".r"),
        buttonLabel = "Load file"
      ),
      downloadButton("scdf_save", "Save scdf"),
    ),

    mainPanel(
      verbatimTextOutput("scdf_summary"),
      verbatimTextOutput("scdf_syntax"),
    )
  )
)

# Transform -----
tab_transform <- tabPanel(
  "Transform",
  sidebarLayout(
    sidebarPanel(
      textInput(
        "select_cases", "Select cases",
        placeholder = "e.g.: 1, Anja, 3:5"
      ),
      div(style="display:inline-block; vertical-align: top",
        textInput("select_phasesA", "Combine phases to A", placeholder = "(e.g.: 1)")
      ),
      div(style="display:inline-block; vertical-align: top; padding-left: 30px;",
        textInput("select_phasesB", "Combine phases to B", placeholder = "(e.g.: 2,3)")
      ),
      
      textInput(
        "subset", "Filter measurments",
        placeholder = 'e.g.: mt > mean(values[phase == "A"])'
      ),
      textAreaInput(
        "transform", "Transform variables", rows = 5,
        placeholder = res$placeholder$transform
      ),
      textInput(
        "setdvar", "Set dependent variable", placeholder = "e.g.: depression"
      ),
      downloadButton("transformed_save", "Save transformed scdf"),
    ),
    mainPanel(
      
      radioButtons(
        "transform_out", "Output format", c("Text", "Html"), inline = TRUE
      ),
      hr(),
      verbatimTextOutput("transform_syntax"),
      conditionalPanel(
        'input.transform_out == "Text"', verbatimTextOutput("transform_scdf")
      ),
      conditionalPanel(
        'input.transform_out == "Html"', htmlOutput("transform_html")
      )
    )
  )
)


# Stats -----
tab_stats <- tabPanel(
  "Stats",
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "func",
        "Statistic",
        choices = res$choices$fn_stats
      ),
      uiOutput("stats_arguments")
    ),
    mainPanel(
      div(
        style="display:inline-block; vertical-align: top",
        radioButtons(
          "stats_out", "Output format", c("Text", "Html"), "Text",
          inline = TRUE
        )
      ),
      div(
        style="display:inline-block; vertical-align: top; padding-left: 30px;",
        textInput(
          "stats_print_arguments", "Output arguments",
          placeholder = "e.g.: flip = TRUE; digits = 2; meta = FALSE"
        )
      ),
      div(
        style="display:inline-block; padding-left: 30px;",
        downloadButton("stats_save", "Save output")
      ),
      hr(),
      verbatimTextOutput("stats_syntax"),
      conditionalPanel(
        'input.stats_out == "Text"', verbatimTextOutput("stats_text")
      ),
      conditionalPanel(
        'input.stats_out == "Html"', tableOutput("stats_html")
      )
    )
  )
)


# Plot -----
tab_plot <- tabPanel(
  "Plot",
  sidebarLayout(
    sidebarPanel(
      textAreaInput(
        "plot_arguments", "Arguments", value = "",rows = 5,
        placeholder = res$placeholder$plot_arguments
      ),
      selectInput("scplot_examples", "stats templates",
          choices = names(res$choices$scplot_examples)
      ),
      selectInput("scplot_templates_design", "design templates",
                    choices = names(res$choices$scplot_templates_design)
      ),
      downloadButton("saveplot", "Save plot", inline = FALSE)
    ),
    mainPanel(
      verbatimTextOutput("plot_syntax"),
      plotOutput("plot_scdf", width = 1000,height = 800,)
    )
  )
)

# Power-test -----
tab_power_test <- tabPanel(
  "Power-test",
  fluidRow(
    column(2, div(style = res$div$pt,
      h3("Design"),
      br(),
      textInput("design_n", "n", value = 1),
      textInput("design_phase", "Phase design", value = "A = 5, B = 15"),
      textInput("design_trend", "Trend", value = "0.02"),
      textInput("design_slope", "Slope", value = "0"),
      textInput("design_level", "Level", value = "1"),
      textInput("design_start", "Start value", value = 50),
      #textInput("design_s", "Standard deviation", value = 10),
      textInput("design_rtt", "Reliabiliy", value = 0.8),
      selectInput("design_distribution", "Distribution", 
                 choices = c("normal", "poisson", "binomial")
      ),
    )),
    column(2, div(style = res$div$pt,
      h3("Analysis"),
      br(),
      checkboxGroupInput(
        "pt_method", "Statistical method(s)", 
        choices = res$choices$pt_method,selected = "plm_level"
      ),
      #selectInput("pt_method", "Statstical method", 
      #           choices = res$choices$pt_method
      #),
      selectInput(
       "pt_effect", "Null effect", choices = c("level", "slope")
      ),
      numericInput(
        "pt_n", "Number of simulations", min = 30, max = 10000, value = 100
      ),
      textInput("pt_ci", "Confidence intervall", placeholder = "e.g.: 0.95"),   
    )),
    column(4, div(style = res$div$pt,
      br(),
      verbatimTextOutput("pt_syntax"),
      actionButton("pt_compute", "Run"),
      hr(),
      verbatimTextOutput("pt_results", placeholder = TRUE)
    )),
    column(4, div(style = res$div$pt,
      actionButton("desigh_plot_button", "Create plot example"),
      plotOutput("plot_design", width = 600, height = 800)
    )),
  ),
 
)

# settings -----
tab_settings <- tabPanel(
  "Settings",
  fluidRow(
    column(2, div(
      style = res$div$settings, 
      h3("Data"),
      #hr(),
      textInput("prefix_output_data", "Prefix output filename", value = "scdf"),
      radioButtons(
       "save_scdf_format", "Save format", 
       choices = c("R object" = ".rds", "R syntax" = ".R", "csv" = ".csv"), 
       inline = TRUE),
      radioButtons(
       "convert", "Code phase structure", 
       choices = c("phase_design" = FALSE, "inline" = TRUE), 
       inline = TRUE
      )
    )),
    column(2, div(
      style = res$div$settings, 
      h3("Transformed"),
      #hr(),
      textInput(
        "prefix_output_transformed", 
        "Prefix output filename", 
        value = "scdf-transformed"
      ),
      radioButtons(
        "save_transformed_format", "Save format", 
        choices = c("R object" = ".rds", "R syntax" = ".R", "csv" = ".csv"), 
        inline = TRUE)
    )),
    column(2, div(
      style = res$div$settings, 
      h3("Stats"),
      #hr(),
      radioButtons(
        "stats_default", "Show defaults", choices = c("No", "Yes"),
        inline = TRUE
      ),
      textInput("prefix_output_stats", "Prefix output filename", value = "scan-stat")
    )),
    column(2, div(
      style = res$div$settings, 
      h3("Plot"),
      #br(),
      #numericInput("plot_display_res", "Display resolution", value = 120, min = 10, max = 4000),
      textInput("prefix_output_plot", "Prefix output filename", value = "scplot"),
      numericInput("width", "Export width", value = 800, min = 100, max = 2000),
      numericInput("height", "Export height", value = 600, min = 100, max = 2000),
      numericInput("dpi", " Export dpi", value = 100, min = 50, max = 600)
    ))
  )
)

# Help -----

tab_help <- tabPanel(
  "Help",
  res$help_page
)


# About -----

tab_about <- tabPanel(
  "About",
  h4("Running:"),
  h4(paste0(
    "scan ",
    utils::packageVersion("scan")," (",utils::packageDate('scan'), ")"
  )),
  h4(paste0(
    "scplot ",
    utils::packageVersion("scplot")," (",utils::packageDate('scplot'), ")"
  )),
  hr(),
  h4("Please cite as:"),
  h4({x<-citation("scan"); class(x)<-"list"; attributes(x[[1]])$textVersion}),
  hr(),
  h4("(c) Jürgen Wilbert, 2023")
)

# ui ------

ui <- navbarPage(
  id = "navpage",
  title = "Shiny-Scan",
  theme = "cerulean.min.css",
  tab_scdf,
  tab_transform,
  tab_stats,
  tab_plot,
  tab_power_test,
  tab_settings,
  tab_help,
  tab_about,
  tabPanel(title = "Quit")
)
