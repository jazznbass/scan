library(shiny)

ui <- fluidPage(
  titlePanel("scdf importer"),
  sidebarLayout(
    sidebarPanel(
      h4("Step 1 — Load file"),
      fileInput("file", "Choose file",
                accept = c(".csv", ".xlsx", ".xls")),
      #radioButtons("type", "Type (auto by default)", inline = TRUE,
      #             choices = c("auto" = "auto", "csv", "excel", "xlsx", "xls"),
      #             selected = "auto"),
      textInput("na", "NA strings (comma-separated)", ', , NA'),
      conditionalPanel(
        "input.type == 'csv' || input.type == 'auto'",
        h5("CSV options"),
        textInput("sep", "sep (leave empty for default)", ""),
        textInput("dec", "dec (leave empty for default)", "")
      ),
      conditionalPanel(
        "input.type == 'excel' || input.type == 'xlsx' || input.type == 'xls'",
        h5("Excel options"),
        numericInput("sheet", "sheet (numeric; optional)", value = NA, min = 1, step = 1)
      ),
      actionButton("load_btn", "Load", class = "btn-primary"),
      tags$hr(),
      h4("Step 2 — Map columns"),
      uiOutput("mapping_ui"),
      checkboxInput("sort_cases", "Sort cases", FALSE),
      textInput("phase_names", "Phase names (comma-separated, optional)", ""),
      actionButton("build_btn", "Build scdf", class = "btn-success"),
      tags$hr(),
      downloadButton("download_rds", "Download SCDF (.rds)")
    ),
    mainPanel(
      h4("Status"),
      verbatimTextOutput("status"),
      h4("Preview"),
      tableOutput("preview"),
      h4("Object structure"),
      verbatimTextOutput("str_out"),
      h4("Reproducible code"),
      verbatimTextOutput("code_out")
    )
  )
)

server <- function(input, output, session) {
  
  `%||%` <- function(x, y) if (is.null(x) || (is.character(x) && !nzchar(x))) y else x
  parse_comma_vec <- function(x) {
    if (!nzchar(x)) return(character())
    trimws(strsplit(x, ",", fixed = TRUE)[[1L]])
  }
  
  # Hold raw table (Step 1) and final scdf (Step 2)
  raw_df <- reactiveVal(NULL)
  scdf_obj <- reactiveVal(NULL)
  last_file_name <- reactiveVal(NULL)
  last_type <- reactiveVal(NULL)
  
  # Step 1: Load ----------------------------------------------------------------
  observeEvent(input$load_btn, {
    req(input$file)
    fpath <- input$file$datapath
    fname <- input$file$name
    ext <- tolower(tools::file_ext(fname))
    type <- "auto" #input$type == "auto"
    typ <- if (type == "auto") {
      if (ext %in% c("yml", "yaml")) ext
      else if (ext %in% c("xlsx","xls")) "excel"
      else if (ext %in% c("csv")) "csv"
      else NA_character_
    } #else input$type
    last_file_name(fname); last_type(typ)
    
    nas <- parse_comma_vec(input$na)
    nas <- if (length(nas)) nas else c("", "NA")
    
    # if (typ %in% c("yml","yaml")) {
    #   # YAML is already structured for read_scdf → import directly, skip mapping
    #   obj <- try(read_scdf(fpath, type = typ), silent = TRUE)
    #   if (inherits(obj, "try-error")) {
    #     showNotification(paste("YAML import failed:", as.character(obj)), type = "error")
    #     return()
    #   }
    #   scdf_obj(obj)
    #   raw_df(NULL)
    #   showNotification(sprintf("Imported %d case(s) from YAML.", length(obj)), type = "message")
    #   return()
    # }
    
    if (identical(typ, "excel") || typ %in% c("xlsx","xls")) {
      if (!requireNamespace("readxl", quietly = TRUE)) {
        showNotification("Please install 'readxl' to load Excel files.", type = "error")
        return()
      }
      sheet_arg <- if (!is.na(input$sheet)) list(sheet = as.integer(input$sheet)) else list()
      tb <- try(do.call(readxl::read_excel, c(list(path = fpath, na = nas), sheet_arg)), silent = TRUE)
      if (inherits(tb, "try-error")) {
        showNotification(paste("Excel import failed:", as.character(tb)), type = "error"); return()
      }
      raw_df(as.data.frame(tb))
      scdf_obj(NULL)
      showNotification("File loaded. Map columns in Step 2.", type = "message")
      return()
    }
    
    # CSV / generic
    args <- list(file = fpath, header = TRUE, stringsAsFactors = FALSE, na.strings = nas)
    if (nzchar(input$sep)) args$sep <- input$sep
    if (nzchar(input$dec)) args$dec <- input$dec
    tb <- try(do.call(utils::read.csv, args), silent = TRUE)
    if (inherits(tb, "try-error")) {
      showNotification(paste("CSV import failed:", as.character(tb)), type = "error"); return()
    }
    raw_df(tb)
    scdf_obj(NULL)
    showNotification("File loaded. Map columns in Step 2.", type = "message")
  })
  
  # Step 2: Mapping UI ----------------------------------------------------------
  output$mapping_ui <- renderUI({
    df <- raw_df()
    if (is.null(df)) {
      # If YAML already imported, mapping does not apply
      if (!is.null(scdf_obj())) return(helpText("YAML imported. Mapping not applicable."))
      return(helpText("Load a CSV/Excel file in Step 1 to enable mapping."))
    }
    cols <- names(df)
    tagList(
      selectInput("cvar", "Case", choices = cols, selected = guess_col(cols, c("case", "id", "subject"))),
      selectInput("pvar", "Phase", choices = cols, selected = guess_col(cols, c("phase", "condition", "segment"))),
      selectInput("dvar", "Values", choices = cols, selected = guess_col(cols, c("values", "score", "y"))),
      selectInput("mvar", "Measurement time", choices = cols, selected = guess_col(cols, c("mt", "time", "t", "day")))
    )
  })
  
  guess_col <- function(cols, hints) {
    hit <- which(tolower(cols) %in% hints)
    if (length(hit)) cols[hit[1]] else cols[1]
  }
  
  # Build SCDF from mapping -----------------------------------------------------
  observeEvent(input$build_btn, {
    if (!is.null(scdf_obj()) && is.null(raw_df())) {
      showNotification("Already imported (YAML).", type = "message")
      return()
    }
    df <- raw_df(); req(df, input$cvar, input$pvar, input$dvar, input$mvar)
    
    phase_names <- parse_comma_vec(input$phase_names)
    phase_names <- if (length(phase_names)) phase_names else NULL
    
    obj <- try(as_scdf(
      object      = df,
      sort_cases  = isTRUE(input$sort_cases),
      cvar        = input$cvar,
      pvar        = input$pvar,
      dvar        = input$dvar,
      mvar        = input$mvar,
      phase_names = phase_names
    ), silent = TRUE)
    
    if (inherits(obj, "try-error")) {
      showNotification(paste("Building SCDF failed:", as.character(obj)), type = "error"); return()
    }
    scdf_obj(obj)
    showNotification(sprintf("Built SCDF with %d case(s).", length(obj)), type = "message")
  })
  
  # Outputs ---------------------------------------------------------------------
  output$status <- renderText({
    if (!is.null(scdf_obj())) {
      paste0("SCDF ready. Cases: ", length(scdf_obj()))
    } else if (!is.null(raw_df())) {
      "File loaded. Map columns and click 'Build SCDF'."
    } else {
      "Awaiting file."
    }
  })
  
  output$preview <- renderTable({
    if (!is.null(scdf_obj())) {
      # show first case head
      #utils::head(scdf_obj()[[1]])
    } else if (!is.null(raw_df())) {
      utils::head(raw_df())
    }
  }, striped = TRUE, spacing = "s")
  
  output$str_out <- renderPrint({
    if (!is.null(scdf_obj())) {
      summary(scdf_obj())
    } else if (!is.null(raw_df())) {
      #utils::str(raw_df())
    }
  })
  
  output$code_out <- renderText({
    # Show a reproducible code snippet for the user’s workflow
    if (!is.null(scdf_obj()) && is.null(raw_df())) {
      # YAML path
      sprintf(
        "dat <- read_scdf(file = %s, type = %s)",
        shQuote(last_file_name() %||% "<your.yml>"),
        shQuote(last_type() %||% "yml")
      )
    } else if (!is.null(scdf_obj()) && !is.null(raw_df())) {
      # Two-step: read flat file then as_scdf mapping
      fname <- last_file_name() %||% "<your.csv/xlsx>"
      typ   <- last_type() %||% "csv"
      na_vec <- parse_comma_vec(input$na)
      na_txt <- if (length(na_vec)) paste0("c(", paste(sprintf('"%s"', na_vec), collapse = ", "), ")") else 'c("", "NA")'
      read_part <-
        if (identical(typ, "excel") || typ %in% c("xlsx","xls")) {
          sheet_txt <- if (!is.na(input$sheet)) sprintf(", sheet = %d", as.integer(input$sheet)) else ""
          sprintf("df <- readxl::read_excel(%s, na = %s%s)\n", shQuote(fname), na_txt, sheet_txt)
        } else {
          sep_txt <- if (nzchar(input$sep)) sprintf(", sep = %s", shQuote(input$sep)) else ""
          dec_txt <- if (nzchar(input$dec)) sprintf(", dec = %s", shQuote(input$dec)) else ""
          sprintf("df <- utils::read.csv(%s, header = TRUE, stringsAsFactors = FALSE, na.strings = %s%s%s)\n",
                  shQuote(fname), na_txt, sep_txt, dec_txt)
        }
      phase <- parse_comma_vec(input$phase_names)
      phase_txt <- if (length(phase)) {
        paste0("c(", paste(sprintf('"%s"', phase), collapse = ", "), ")")
      } else "NULL"
      paste0(
        read_part,
        "dat <- as_scdf(\n",
        "  object = as.data.frame(df),\n",
        "  cvar = ", shQuote(input$cvar), ", pvar = ", shQuote(input$pvar), ",\n",
        "  dvar = ", shQuote(input$dvar), ", mvar = ", shQuote(input$mvar), ",\n",
        "  sort_cases = ", isTRUE(input$sort_cases), ",\n",
        "  phase_names = ", phase_txt, "\n",
        ")"
      )
    } else ""
  })
  
  # Download --------------------------------------------------------------------
  output$download_rds <- downloadHandler(
    filename = function() {
      base <- last_file_name() %||% "scdf"
      paste0(tools::file_path_sans_ext(basename(base)), ".rds")
    },
    content = function(path) {
      req(scdf_obj()); saveRDS(scdf_obj(), path)
    }
  )
}

shinyApp(ui, server)
