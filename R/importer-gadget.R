#' Import scdf – RStudio Addin
#' @export
import_scdf <- function() {
  #suppressPackageStartupMessages({
  #  library(shiny)
  #  library(bslib)
  #})

  ui <- miniUI::miniPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
      body { zoom: 0.8; }
    "))
    ),
    miniUI::gadgetTitleBar("scdf import"),
    miniUI::miniContentPanel(
      shiny::verbatimTextOutput("status"),
      shiny::fillRow(
        flex = c(1, 1),
        shiny::wellPanel(
          shiny::fileInput("file", "Choose file",
                           accept = c(".csv", ".xlsx", ".xls", ".rds")),
          shiny::textInput("na", "NA strings (comma-sep.)", '"", NA'),
          shiny::selectInput("sep", "CSV separator", 
                      c("comma" =",", "semicolon" =";", "tab" = "\t"), selected = 1),
          shiny::textInput("obj_name", "Save as (object name)", "dat_scdf")
        ),
        shiny::wellPanel(
          shiny::uiOutput("map_ui")
        )
      ),
      shiny::tableOutput("preview")
    )
  )
  
  server <- function(input, output, session) {
    raw_df  <- shiny::reactiveVal(NULL)
    scdfobj <- shiny::reactiveVal(NULL)
    fname   <- shiny::reactiveVal(NULL)
    ftype   <- shiny::reactiveVal(NULL)
    filepath <- shiny::reactiveVal()
    
    output$status <- shiny::renderText("First choose a file")
    
    
    
    shiny::observeEvent(input$file, {
      path <- input$file$datapath
      fname(input$file$name)
      ext  <- tolower(tools::file_ext(input$file$name))
      typ  <- "auto"
      ftype(typ)
      filepath(path)
      nas <- eval(str2lang(paste0("c(",input$na, ")")))
      
      # rds
      
      if (ext == "rds") {
        tb <- try(
          readRDS(path), silent = TRUE
        )
        if (inherits(tb, "try-error")) { 
          output$status <- shiny::renderText(as.character(tb))
          return() 
        }
        if (inherits(tb, "scdf")) {
          if (rstudioapi::isAvailable()) {
            tmp <- tempfile(pattern = "scdf_", fileext = ".rds")
            saveRDS(tb, tmp)
            rstudioapi::sendToConsole(
              sprintf('%s <- readRDS("%s")', input$obj_name, tmp),
              execute = TRUE
            )
          }
          shiny::stopApp(tb)
        } else {
          output$status <- shiny::renderText("Not a valid scdf file")
          return()
        }
       
      }
      
      # Excel
      if (ext %in% c("xlsx","xls")) {
        tb <- try(
          readxl::read_excel(path = path, na = nas), silent = TRUE
        )
        if (inherits(tb, "try-error")) { 
          output$status <- shiny::renderText(as.character(tb))
          return() 
        }
        raw_df(as.data.frame(tb))
      }
      
      # CSV
      if (ext %in% c("csv","txt")) {
        args <- list(file = path, header = TRUE, stringsAsFactors = FALSE, na.strings = nas)
        sep <- input$sep
        tb <- try(do.call(utils::read.csv, args), silent = TRUE)
        if (inherits(tb, "try-error")) { 
          output$status <- shiny::renderText(as.character(tb))
          return() 
        }
        raw_df(tb)
      }
      scdfobj(NULL)
      output$status <- shiny::renderText("Loaded table. Map columns and click Done.")
    })
    
    # Mapping UI
    output$map_ui <- shiny::renderUI({
      df <- raw_df()
      if (is.null(df)) return()#shiny::helpText("Not applicable for rds or file not loaded."))
      cols <- names(df)
      sel <- function(hints) { 
        hit <- which(tolower(cols) %in% hints)
        if (length(hit)) cols[hit[1]] else cols[1] 
      }
      shiny::tagList(
        shiny::selectInput("cvar", "Case",  cols, selected = sel(c("case", "id", "subject"))),
        shiny::selectInput("pvar", "Phase", cols, selected = sel(c("phase","condition","segment"))),
        shiny::selectInput("dvar", "Dependent variable",cols, selected = sel(c("values","score","y", "outcome"))),
        shiny::selectInput("mvar", "Mesaurement-time",    cols, selected = sel(c("mt","time","t","day", "session")))
      )
    })
    
    # Preview
    output$preview <- shiny::renderTable({
      if (!is.null(scdfobj())) utils::head(scdfobj()[[1]])
      else if (!is.null(raw_df())) utils::head(raw_df())
    }, striped = TRUE, spacing = "s")
    
    # Handle Done/Cancel
    shiny::observeEvent(input$done, {
      nm <- input$obj_name
      # Build from mapping if needed
      df <- raw_df()
      if (!is.null(df) && all(nzchar(c(input$cvar, input$pvar, input$dvar, input$mvar)))) {
        obj <- try(as_scdf(
          object = df,
          cvar = input$cvar, pvar = input$pvar, dvar = input$dvar, mvar = input$mvar
        ), silent = TRUE)
        if (!inherits(obj, "try-error")) {
          
          # code <- paste0(
          #   nm, " <- read_scdf(", deparse(input$file$datapath), ", ",
          #   "cvar = ",  deparse(input$cvar), ", ",
          #   "pvar = ",  deparse(input$pvar), ", ",
          #   "dvar = ",  deparse(input$dvar), ", ",
          #   "mvar = ",  deparse(input$mvar), ", ",
          #   "na = c(", input$na,
          # ")")
          
          if (rstudioapi::isAvailable()) {
            tmp <- tempfile(pattern = "scdf_", fileext = ".rds")
            saveRDS(obj, tmp)
            rstudioapi::sendToConsole(
              sprintf('%s <- readRDS("%s")', nm, tmp),
              execute = TRUE, echo = FALSE 
            )
            
            rstudioapi::sendToConsole(nm, execute = FALSE)
          }
            
        }
      }
      shiny::stopApp(obj)
    })
    shiny::observeEvent(input$cancel, shiny::stopApp(invisible(NULL)))
  }
  
  viewer <- shiny::dialogViewer("scdf import", width = 400, height = 400)
  shiny::runGadget(ui, server, viewer = viewer)
}
