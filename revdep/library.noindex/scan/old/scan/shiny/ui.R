source("resources.R")

# data ------
tab_scdf <- tabPanel(
  "New",
  sidebarLayout(
    sidebarPanel(
      textAreaInput(
        "values", 
        "Values (dependent variable)", 
        placeholder = res$placeholder$values
      ),
      textInput("mt", "Measurement times", placeholder = res$placeholder$mt),
      textAreaInput(
        "variables", "Additional variables",
        placeholder = res$placeholder$variables
      ),
      textInput("casename", "Case name", placeholder = "(optional)"),
      actionButton("add_case", "Add case"),
      actionButton("remove_case", "Remove case"),
      actionButton("clear_cases", "Clear all cases"),
      br(),
      div(style="display:inline-block;",
          radioButtons(
            "remove_which", 
            "Position", 
            choices = c("last" = "last", "at:" = "at"), 
            inline = TRUE
          ),
      ),
      div(style="display:inline-block; padding-left: 0px",
          numericInput("remove_at", "", min = 1,value = 1,width = "75px"),
      ),
      br(),
      downloadButton("scdf_save", "Save scdf"),
    ),

    mainPanel(
      verbatimTextOutput("scdf_messages"),
      verbatimTextOutput("scdf_output")
    )
  )
)

tab_load <- tabPanel(
  "Load",
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "upload", NULL, accept = c(".csv", ".rds", ".xlsx", ".xls", ".R", ".r"),
        buttonLabel = "Open file"
      ),
      selectInput(
        "scdf_example", "Choose example", choices = res$choices$examples,
      ),
    ),
    
    mainPanel(
      verbatimTextOutput("load_messages"),
      verbatimTextOutput("load_output")
      #htmlOutput("load_html_output")
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
        textInput(
          "select_phasesA", 
          "Combine phases to A", 
          placeholder = "(e.g.: 1)"
        )
      ),
      div(style="display:inline-block; vertical-align: top; padding-left: 30px;",
        textInput(
          "select_phasesB", 
          "Combine phases to B", 
          placeholder = "(e.g.: 2,3)"
        )
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
      verbatimTextOutput("transform_syntax"),
      conditionalPanel(
        'input.transform_output_format == "Text"', 
        verbatimTextOutput("transform_scdf")
      ),
      conditionalPanel(
        'input.transform_output_format == "Html"', htmlOutput("transform_html")
      )
    )
  )
)

navbar_data <- navbarMenu(
  "Data",
  tab_scdf,
  tab_load#,
  #tabPanel(downloadButton("test_save", "Save", class = "align: left;")),
  #tab_transform
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
          "stats_print_arguments", 
          "Output arguments",
          placeholder = res$placeholder$stats_out_args
        )
      ),
      div(
        style="display:inline-block; vertical-align: top; padding-left: 30px;",
        radioButtons(
          "stats_export_flip", 
          "Flip",
          c(FALSE, TRUE),
          inline = TRUE
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
      selectInput("scplot_examples", "Stats templates",
          choices = names(res$choices$scplot_examples)
      ),
      selectInput("scplot_templates_design", "Design templates",
                    choices = names(res$choices$scplot_templates_design)
      ),
      selectInput("scplot_templates_annotate", "Annotate templates",
                  choices = names(res$choices$scplot_templates_annotate)
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
      h3("Case design"),
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
        "pt_method", "Method(s)", 
        choices = res$choices$pt_method,selected = "plm_level"
      ),
      textInput("pt_method_user", "User method", value = ""),
      selectInput(
       "pt_effect", "Null effect for", choices = c("level", "slope")
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
      radioButtons(
        "scdf_output_format", "Show output as", 
        choices = c("Summary", "Syntax"), 
        inline = TRUE
      ),
      radioButtons(
        "scdf_syntax_phase_structure", "Syntax phase structure", 
        choices = c("phase_design" = FALSE, "inline" = TRUE), 
        inline = TRUE
      ),
      textInput("scdf_save_prefix", "Prefix save filename", value = "scdf"),
      radioButtons(
       "scdf_save_format", "Save format", 
       choices = c("R object" = ".rds", "R syntax" = ".R", "csv" = ".csv"), 
       inline = TRUE),
      textInput("scdf_load_na", "Missing values in import file", value = '"", "NA"'),

    )),
    column(2, div(
      style = res$div$settings, 
      h3("Transform"),
      radioButtons(
        "transform_output_format", "Show output as", c("Text", "Html"), inline = TRUE
      ),
      textInput(
        "transform_save_prefix", 
        "Prefix save filename", 
        value = "scdf-transformed"
      ),
      radioButtons(
        "transform_save_format", "Save format", 
        choices = c("R object" = ".rds", "R syntax" = ".R", "csv" = ".csv"), 
        inline = TRUE)
    )),
    column(2, div(
      style = res$div$settings, 
      h3("Stats"),
      radioButtons(
        "stats_default", "Syntax with defaults", choices = c("No", "Yes"),
        inline = TRUE
      ),
      radioButtons(
        "format_output_stats", "Save format", choices = c("html", "docx", "text"),
        inline = TRUE
      ),
      textInput("prefix_output_stats", "Prefix save filename", value = "scan-stat")
    )),
    column(2, div(
      style = res$div$settings, 
      h3("Plot"),
      textInput("prefix_output_plot", "Prefix save filename", value = "scplot"),
      numericInput("width", "Export width", value = 1600, min = 100, max = 4000),
      numericInput("height", "Export height", value = 1200, min = 100, max = 4000),
      numericInput("dpi", " Export dpi", value = 200, min = 50, max = 1200)
    )),
    column(2, div(
      style = res$div$settings, 
      h3("General"),
      radioButtons(
        "scan_export_engine", "Html engine", choices = c("gt", "kable"),
        inline = TRUE
      )
    ))
  )
)

# Help -----

navbar_help <- navbarMenu(
  "Help",
  tabPanel("Info",res$help_page),
  tabPanel(
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
  ),
  tabPanel(title = "Quit")
)

# ui ------

js <- HTML(
  "Shiny.addCustomMessageHandler('closeWindow', function(data) {
    eval(data.message)
  });"
)
  
ui <- navbarPage(
  id = "navpage",
  title = "Shiny-Scan",
  theme = "cerulean.min.css",
  tags$head(tags$script(js)),
  navbar_data,#tab_scdf,
  tab_transform,
  tab_stats, #navbar_analyses,
  tab_plot,
  tab_power_test,
  tab_settings,
  navbar_help
)
