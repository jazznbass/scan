
source("resources.R")

left_width <- 400

# ---------- Data: New ----------

tab_scdf <- nav_panel(
  "New / Edit",
  layout_sidebar(
    sidebar = sidebar(
      title = NULL,
      open  = "always", width = left_width,
      selectInput("new_select_case", "Select case",
                  choices = res$new_case, selected = res$new_case),
      textAreaInput("new_values", "Values (dependent variable)",
                    placeholder = res$placeholder$values, rows = 3),
      textInput("new_mt", "Measurement times", placeholder = res$placeholder$mt),
      textAreaInput("new_variables", "Additional variables",
                    placeholder = res$placeholder$variables, rows = 3),
      textInput("new_casename", "Case name", 
                placeholder = res$placeholder$casename),
      
      div(class = "d-grid gap-2",
          actionButton("new_save_case",   "Save case",    class = "btn-primary"),
          actionButton("new_remove_case", "Remove case",  class = "btn-light"),
          actionButton("new_clear_cases", "Clear all cases",
                       class = "btn-warning"),
          actionButton("new_clear_fields", "Clear input fields",
                       class = "btn-dark")),
      
      tags$hr(),
      downloadButton("scdf_save", "Save scdf", class = "btn-success")
    ),
    
    # main content
    mainPanel(
      verbatimTextOutput("scdf_messages"),
      verbatimTextOutput("scdf_output")
    )
  )
)

# ---------- Data: Load ----------
card_load_example <- card(
  card_body(
    selectInput("scdf_example", "Choose example", 
                choices = res$choices$examples, size = 15,
                selectize = FALSE),
    tags$hr(),
    actionButton("scdf_import", "Import example", class = "btn-primary")
  )
)
  
card_load_import <- card(
  card_body(
    fileInput("upload", NULL,
              accept = c(".csv", ".rds", ".xlsx", ".xls", ".R", ".r", ".txt"),
              buttonLabel = "Choose file"),
    selectInput("scdf_load_cvar", "Case variable", choices = "case"),
    selectInput("scdf_load_pvar", "Phase variable", choices = "phase"),
    selectInput("scdf_load_dvar", "Dependent variable", choices = "values"),
    selectInput("scdf_load_mvar", "Measurement time variable", choices = "mt"),
    textInput("scdf_load_na", "Missing values", value = '"", "NA"'),
    selectInput("scdf_csv", "Separators (only for .csv and .txt files)", 
                choices = res$choices$separators),
    tags$hr(),
    actionButton("scdf_import",   "Import scdf",    class = "btn-primary")
  )
)

card_load_output <- card(
  card_body(
    verbatimTextOutput("load_messages"),
    verbatimTextOutput("load_output"),
    htmlOutput("load_output_html")
  )
)
  
nav_panel_load <- nav_panel("Load",
  layout_columns(
    col_widths = c(3, 9),
    navset_tab(
      nav_panel("Import", card_load_import),
      nav_panel("Examples", card_load_example)
    ),
    card_load_output
))

# ---------- Transform ----------
tab_transform <- layout_sidebar(
  sidebar = sidebar(
    title = NULL, # title = "Transform",
    open  = "always", width = left_width,
    textInput("select_cases", "Select cases", placeholder = "e.g.: 1, Anja, 3:5"),
    div(class = "d-flex gap-3",
        textInput("select_phasesA", "Combine phases to A", placeholder = "(e.g.: 1)"),
        textInput("select_phasesB", "Combine phases to B", placeholder = "(e.g.: 2,3)")
    ),
    textInput("subset", "Filter measurements", 
              placeholder = 'e.g.: mt > mean(values[phase == "A"])'),
    textAreaInput("transform", "Transform variables", rows = 5, 
                  placeholder = res$placeholder$transform),
    selectInput("setdvar", "Set dependent variable", choices = ""),
    downloadButton("transformed_save", "Save transformed scdf", class = "btn-success")
  ),
  mainPanel(
    input_switch("transform_output_format", "HTML", value = FALSE),
    verbatimTextOutput("transform_syntax"),
    conditionalPanel(
      '!input.transform_output_format', verbatimTextOutput("transform_scdf")
    ),
    conditionalPanel( 'input.transform_output_format', htmlOutput("transform_html"))
  )
)

# ---------- Stats ----------

card_stats_main <- card(
  card_body(
    h5("Select case"),
    selectInput("stats_select_case", NULL, choices = "all"),
    h5("Select analysis"),
    selectInput("stats_func", NULL, choices = res$choices$fn_stats,
                selectize = FALSE, size = length(res$choices$fn_stats))
  )
)

card_stats_args <- card(
  card_body(
    uiOutput("stats_arguments")
  )
)

card_stats_output <- card(
  card_body(
    tags$head(tags$style(HTML("
      .toolbar { display:flex; gap:16px; align-items:center; }
      .toolbar .stretch { flex:1; }"
    ))),
    div(class = "toolbar",
        div(class = "stretch", textAreaInput(
          "stats_print_arguments", label = "Output arguments",
          rows = 1, width = "100%", placeholder = res$placeholder$stats_out_args
        )),
        downloadButton("stats_save", label = "Save", class = "btn-success")
    ),
    div(class = "toolbar",
        input_switch("stats_out", "HTML", TRUE),
        input_switch("stats_description","Method description", FALSE)
    ), 
    conditionalPanel('input.stats_description', htmlOutput("stats_description")),
    verbatimTextOutput("stats_syntax"),
    conditionalPanel('!input.stats_out', verbatimTextOutput("stats_text")),
    conditionalPanel('input.stats_out', tableOutput("stats_html"))
  )
)

tab_stats <- layout_columns(
  col_widths = c(3, 9),
  navset_tab(
    id = "stats_tabs",
    nav_panel("Main", card_stats_main),
    nav_panel("Arguments", card_stats_args)
  ),
  card_stats_output
)


# ---------- Plot -------

card_plot_theme <- card(
  card_body(
    h5("Select case"),
    selectInput("scplot_select_case", NULL, choices = "all"),
    
    h5("Themes"),
    selectInput("scplot_theme_1", "Complete", 
                choices = res$scplot_themes_complete, selected = "default"),
    selectInput("scplot_theme_2", "Add element", 
                choices = res$scplot_themes_element),
    selectInput("scplot_theme_3", "Add element", 
                choices = res$scplot_themes_element),
    h5("Legend position"),
    selectInput("scplot_legend_position", NULL,
                choices = res$choices$legend_position, 
                selected = "none"),
    h5("Textsize"),
    sliderInput("scplot_text_size", NULL, min = 6, max = 25, value = 6)
  )
)

card_plot_axis <- card(
  card_body(
    h5("Axis"),
    numericInput("scplot_xinc", "x increment", value = NA),
    layout_columns(
      col_widths = c(6, 6),
      numericInput("scplot_ymin", "y min", value = NA),
      numericInput("scplot_ymax", "y max", value = NA)
    ),
    textInput("scplot_xlabel", "x label"),
    textInput("scplot_ylabel", "y label"),
    textInput("scplot_phasenames", "Phasenames"),
    textInput("scplot_casenames", "Casenames"),
    input_switch("scplot_footer", "Footnote", FALSE),
    
  )
)

card_plot_lines <- card(
  card_body(
    h5("Baseline"),
    input_switch("scplot_stats_trend_a", "Trend", FALSE),
    input_switch("scplot_stats_mean_a", "Mean", FALSE),
    input_switch("scplot_stats_median_a", "Median", FALSE),
    input_switch("scplot_stats_max_a", "Max", FALSE),
    input_switch("scplot_stats_min_a", "Min", FALSE),
    
    h5("Phases"),
    input_switch("scplot_stats_mean", "Mean", FALSE),
    input_switch("scplot_stats_median", "Median", FALSE),
    input_switch("scplot_stats_trend", "Slope", FALSE),
    
    h5("Curves"),
    input_switch("scplot_stats_moving", "Moving mean", FALSE),
    input_switch("scplot_stats_loess", "Smoothed line", FALSE),
    h5("Add variable"),
    selectInput("scplot_add", NULL, choices = "None")
  )
)

card_plot_save <- card(
  card_body(
    layout_columns(
      col_widths = c(4, 4, 4),
      numericInput("width",  "Width",  value = 1600, min = 100, max = 4000),
      numericInput("height", "Height", value = 1200, min = 100, max = 4000),
      numericInput("dpi",    "Dpi",    value = 200,  min = 50,  max = 1200),
    ),
    downloadButton("saveplot_2", "Save plot", class = "btn-success")
  )
)

card_plot_output <- card(
  card_body(
    #verbatimTextOutput("scplot_syntax"),
    plotOutput("scplot_plot")
  ), height = "100vh"
)


tab_plot <- layout_columns(
  col_widths = c(3, 9),
  navset_tab(
    nav_panel("Theme", card_plot_theme),
    nav_panel("Axis", card_plot_axis),
    nav_panel("Lines", card_plot_lines),
    nav_panel("Save", card_plot_save)
  ),
  card_plot_output
)


# ---------- Power-test ----------

card_design <- card(
  card_body(
    textInput("design_n", "n", value = 1),
    textInput("design_phase", "Phase design", value = "A = 5, B = 15"),
    textInput("design_trend", "Trend", value = "0.02"),
    textInput("design_slope", "Slope", value = "0"),
    textInput("design_level", "Level", value = "1"),
    textInput("design_start", "Start value", value = 50),
    textInput("design_rtt", "Reliabiliy", value = 0.8),
    selectInput("design_distribution", "Distribution", 
                choices = c("normal", "poisson", "binomial"))
  )
)

card_analysis <- card(
  card_body(
    checkboxGroupInput("pt_method", "Method(s)", 
                       choices = res$choices$pt_method, selected = "plm_level"),
    textInput("pt_method_user", "User method", value = ""),
    selectInput("pt_effect", "Null effect for", choices = c("level", "slope")),
    numericInput("pt_n", "Number of simulations", min = 30, max = 10000, value = 100),
    textInput("pt_ci", "Confidence interval", placeholder = "e.g.: 0.95")
  )
)

card_run <- card(
  card_body(
    verbatimTextOutput("pt_syntax"),
    input_task_button("pt_compute", "Run"),
    hr(),
    verbatimTextOutput("pt_results", placeholder = TRUE)
  ), height = "50vh"
)

card_plot <- card(
  card_body(
    actionButton("desigh_plot_button", "Create plot example"),
    plotOutput("plot_design")#, width = 800, height = 800)
  ), height = "50vh"
)


 
tab_power_test <- layout_columns(
  col_widths = c(2, 3, 7),
  heights_equal = "row",
  card_design,
  card_analysis,
  div(class = "d-flex flex-column gap-3 h-100",
    card_run,
    card_plot
  )
)

#tab_power_test <- navset_tab(nav_panel("Power test", tab_power_test))

# ---------- Settings ----------
tab_settings <- layout_columns(
  col_widths = c(2,2,2,2),
  card(
    card_header("Data"),
    card_body(
      input_switch("scdf_output_format", "Show scdf syntax", value = FALSE),
      input_switch("scdf_syntax_phase_structure",
                   tags$span("Phase structure inline", class = "chklabel-big"),
                   value = FALSE),
      textInput("scdf_save_prefix", "Prefix save filename", value = "scdf"),
      radioButtons("scdf_save_format", "Save format",
                   choices = c("R object" = ".rds", "R syntax" = ".R", "csv" = ".csv"),
                   inline = TRUE)
    )
  ),
  card(
    card_header("Transform"),
    card_body(
      textInput("transform_save_prefix", "Prefix save filename",
                value = "scdf-transformed"),
      radioButtons("transform_save_format", "Save format",
                   choices = c("R object" = ".rds", "R syntax" = ".R", "csv" = ".csv"),
                   inline = TRUE)
    )
  ),
  card(
    card_header("Stats"),
    card_body(
      input_switch("stats_default",
                   tags$span("Show defaults", class = "chklabel-big"),
                   value = FALSE),
      radioButtons("rename_predictors", "Rename predictors",
                   choices = c("full", "concise", "no"), inline = TRUE),
      input_switch("setting_output_docx", "Save html as docx", value = FALSE),
      textInput("prefix_output_stats", "Prefix save filename", value = "scan-stat")
    )
  ),
  card(
    card_header("Plot"),
    card_body(
      textInput("prefix_output_plot", "Prefix save filename", value = "scplot"),
      #numericInput("width",  "Export width",  value = 1600, min = 100, max = 4000),
      #numericInput("height", "Export height", value = 1200, min = 100, max = 4000),
      #numericInput("dpi",    "Export dpi",    value = 200,  min = 50,  max = 1200)
    )
  ),
  card(
    card_header("General"),
    card_body(
      radioButtons("scan_export_engine", "Html engine",
                   choices = c("gt", "kable"), inline = TRUE)
    )
  )
)

# ---------- Help ----------
navbar_help <- nav_menu(
  title = "Help",
  nav_panel("Info", res$help_page),
  nav_panel(
    "About",
    h4("Running:"),
    h4(paste0("scan ",   utils::packageVersion("scan"),   " (", 
              utils::packageDate('scan'),   ")")),
    h4(paste0("scplot ", utils::packageVersion("scplot"), " (", 
              utils::packageDate('scplot'), ")")),
    hr(),
    h4("Please cite as:"),
    h4({
      x <- citation("scan")
      class(x) <- "list"
      attributes(x[[1]])$textVersion
    }),
    hr(),
    h4("(c) Jürgen Wilbert, 2026")
  ),
  nav_panel(title = "Quit")
)

# ---------- UI ----------
ui <- tagList(
  tags$head(tags$link(rel = "icon", type = "image/png", href = "logo.png")),
  page_navbar(
    title = "scan",
    id = "scan",
    theme = res$theme_light,
    navbar_options = navbar_options(class = "bg-primary", theme = "dark"),
    #nav_menu("Data", nav_panel_load, tab_scdf),
    nav_panel("Data", nav_panel_load),
    nav_panel("New/Edit", tab_scdf),
    nav_panel("Transform",  tab_transform),
    nav_panel("Stats",      tab_stats),
    nav_panel("Plot",       tab_plot),
    nav_panel("Power",      tab_power_test),
    nav_panel("Settings",   tab_settings),
    navbar_help,
    nav_spacer(),
    
    nav_item(input_switch("darkmode", "Dark mode", value = FALSE))
  )
)
