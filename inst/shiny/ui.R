source("resources.R")

# scdf ------
tab_scdf <-   tabPanel(
  "scdf",
  sidebarLayout(
    sidebarPanel(
      #h4("New case"),
      #br(),
      textAreaInput(
        "values", "Values", placeholder = res$placeholder$values
      ),
      textInput("mt", "Measurement times", placeholder = res$placeholder$mt),
      textAreaInput(
        "variables", "Additional variables",
        placeholder = res$placeholder$variables
      ),
      textInput("casename", "Case name", placeholder = "(optional)"),
      actionButton("add_case", "Add"),
      actionButton("remove_case", "Remove last"),
      actionButton("remove_all", "Remove all"),
      hr(),
      radioButtons(
        "save_scdf_format", "Save format", 
        choices = c("R object" = ".rds", "R syntax" = ".R"), 
        inline = TRUE),
      div(style="display:inline-block; vertical-align: top",
          downloadButton("scdf_save", "Save")
      ),
      div(
        style="display:inline-block; vertical-align: top; padding-left: 20px;",
        fileInput(
          "upload", NULL, accept = c(".csv", ".rds", ".xlsx", "xls"),
           buttonLabel = "Load file"
        )
      ),
      selectInput(
        "scdf_example", "Load example", choices = res$choices$examples,
      ),
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
      textInput(
        "select_phases", "Recombine phases",
        placeholder = "e.g.: A = 1, B = c(2,3)"
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
      downloadButton("transform_save", "Save")
    ),
    mainPanel(
      verbatimTextOutput("transform_syntax"),
      verbatimTextOutput("transform_scdf")
      #htmlOutput("transform_html")
    )
  )
)


# Stats -----
tab_stats <- tabPanel(
  "Stats",
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = res$java$window.open, functions = 'openURL'),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "func",
        "Statistic",
        choices = res$choices$fn_stats
      ),
      radioButtons(
        "stats_default", "Show defaults", choices = c("No", "Yes"),
        inline = TRUE
      ),

      uiOutput("stats_arguments"),
      hr(),
      actionButton("stats_help", "Open help")
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
      hr(),
      verbatimTextOutput("stats_syntax"),
      conditionalPanel(
        'input.stats_out == "Text"', verbatimTextOutput("stats_text")
      ),
      conditionalPanel(
        'input.stats_out == "Html"', htmlOutput("stats_html")
      )

    )
  )
)


## Plot -----
tab_plot <- tabPanel(
  "Plot",
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(text = res$java$window.open, functions = 'openURL'),
  sidebarLayout(
    sidebarPanel(
      #selectInput("plot", "Plot engine", choices = res$choices$fn_plot),
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
      actionButton("plot_help", "Open help", inline = TRUE),
      downloadButton("saveplot", "Save plot", inline = TRUE),
      numericInput("width", "Width", value = 800, min = 100, max = 2000),
      numericInput("height", "Height", value = 600, min = 100, max = 2000),
      numericInput("dpi", "Dpi", value = 100, min = 50, max = 600)
    ),
    mainPanel(
      verbatimTextOutput("plot_syntax"),
      plotOutput("plot_scdf", width = 800,height = 600)
    )
  )
)

## Help -----

tab_help <- tabPanel(
  "Help",
  htmltools::includeMarkdown(res$help_page)
)


## About -----

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

## ui ------

ui <- navbarPage(
  title = "Shiny scan",
  theme = shinythemes::shinytheme("cerulean"),
  #header = shinythemes::themeSelector(),
  tab_scdf,
  tab_transform,
  tab_stats,
  tab_plot,
  tab_help,
  #tab_test,
  tab_about
)
