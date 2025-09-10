
server <- function(input, output, session) {

  session$onFlushed(function() {
    #if (is.null(getOption("scan.shinyscan.initial")))
    #  updateNavbarPage(session, "scan", selected = "Load")
    if (!is.null(getOption("scan.shinyscan.initial")))
      updateNavbarPage(session, "scan", selected = "Stats")
    
  }, once = TRUE)
  
  observeEvent(input$darkmode, {
    session$setCurrentTheme(if (isTRUE(input$darkmode)) res$theme_dark else res$theme_light)
  }, ignoreInit = TRUE)
  
  
  observeEvent(input$scan, {
    if (input$scan == "Plot") {
      choices <- lapply(transformed(), function(x) names(x)) |> unlist() |> unique()
      id <- which(!choices %in% scdf_attr(transformed())[c("var.values", "var.mt", "var.phase")] |> unlist())
      updateSelectInput(
        session, 
        inputId = "scplot_add", 
        choices = c("None", choices[id])
      )
      choices <- correct_casenames(transformed(), string = TRUE)
      choices <- setNames(1:length(choices), choices)
      updateSelectInput(
        session, 
        inputId = "scplot_select_case", 
        choices = c("all", choices)
      )
    }
    if (input$scan == "Stats") {
      choices <- correct_casenames(transformed(), string = TRUE)
      choices <- setNames(1:length(choices), choices)
      updateSelectInput(
        session, 
        inputId = "stats_select_case", 
        choices = c("all", choices)
      )
    }
  })
  
  # scdf ----
  
  ## startup message ----
  
  output$scdf_messages <- renderPrint({
    if (is.null(getOption("scan.shinyscan.initial"))) {
      cat(res$msg$startup)
    }
  
  })

  ## Render ----
  my_scdf <- reactiveVal(getOption("scan.shinyscan.initial", NULL))
  import_file <- reactiveVal()
  
  scdf_render <- reactive({
    
    if (!input$scdf_output_format){
      output$scdf_output <- renderPrint({
        if (!inherits(my_scdf(), "scdf")) validate(res$msg$no_case)
        if (identical(length(my_scdf()), 0)) validate(res$msg$no_case)
        do.call("summary", list(my_scdf()))
      })
    } else if (input$scdf_output_format) {
      output$scdf_output <- renderPrint({
        if (!inherits(my_scdf(), "scdf")) validate(res$msg$no_case)
        if (identical(length(my_scdf()), 0)) validate(res$msg$no_case)
        do.call("convert", list(
          my_scdf(), inline = as.logical(input$scdf_syntax_phase_structure)
        ))
      })
    }
    
    choices <- c(res$new_case, correct_casenames(my_scdf(), string = TRUE))
    updateSelectInput(session, "new_select_case", choices = choices)
  })

  observeEvent(input$new_clear_fields, {
    updateTextAreaInput(session, "new_values", value = "")
    updateTextInput(session, "new_mt", value = "")
    updateTextAreaInput(session, "new_variables", value = "")
    updateTextInput(session, "new_casename", value = "")
  }) 
  
  
  observeEvent(input$scdf_output_format, scdf_render()) 
  
  output$load_messages <- renderPrint({
    if (is.null(getOption("scan.shinyscan.initial"))) {
      cat(res$msg$startup)
    }
  })
  
  
  ## summary render ----
  render_summary <- reactive({
    tmp <- capture.output(out <- summary(import_file()) |> export())
    if (input$scan_export_engine == "gt")
      out <- gt::as_raw_html(out)
    HTML(out)
  })
  
  ## input example ----
  observeEvent(input$scdf_example, {
    if (input$scdf_example != "(none)") {
      import_file(paste0("scan::", input$scdf_example) |> str2lang() |> eval())
      scdf_render()
      msg <- paste("Example", input$scdf_example, 
                 "selected. Press 'Import scdf' to import it.")
      output$load_messages <- renderPrint(cat(msg))
      #showNotification(msg, type = "message")

      output$load_output_html <- renderUI({
        render_summary()
      })
     
      output$scdf_messages <- renderPrint(cat(""))
    } else {
      import_file(NULL)
    }
  })


  ## upload (load) ------
  
  load_file <- function () {
    
    if (length(input$upload$datapath) == 0) return()
    ext <- tolower(tools::file_ext(input$upload$datapath))
    
    if (ext == "rds") {
      readRDS(input$upload$datapath) |> import_file()
    } 
    
    if (ext == "r") {
      new <- readLines(input$upload$datapath)
      new <- paste0(new, collapse = "\n")
      .tmp <- new.env()
      eval(parse(text = new), envir = .tmp) |> import_file()
    } 
    
    if (ext %in% c("csv", "txt")) {
      sep <- input$scdf_csv
      dec <- if (sep == ";") "," else "."
      utils::read.table(
        input$upload$datapath, 
        header = TRUE, 
        stringsAsFactors = FALSE, 
        sep = sep,
        dec = dec,
        quote = "\"", 
        fill = TRUE, 
        comment.char = ""
      ) |> import_file()
    }
    
    if (ext %in% c("xlsx", "xls")) {
      readxl::read_excel(
        input$upload$datapath
      ) |>  
        as.data.frame() |> 
        import_file()
    }
    
    msg <- "File selected. Press 'Import scdf' to import it"
    output$load_messages <- renderText(msg)
    showNotification(msg, type = "message")
    
    if (inherits(import_file(), "data.frame")) {
      choices <- names(import_file())
      updateSelectInput(session, "scdf_load_cvar", choices = choices, 
                        selected = guess_col(choices, c("case", "id", "subject", "name")))
      updateSelectInput(session, "scdf_load_pvar", choices = choices, 
                        selected = guess_col(choices, c("phase", "condition", "segment")))
      updateSelectInput(session, "scdf_load_mvar", choices = choices, 
                        selected = guess_col(choices, c("measurement", "mt", "time", "t", "day", "session")))
      updateSelectInput(session, "scdf_load_dvar", choices = choices, 
                        selected = guess_col(choices, c("values", "score", "y", "outcome")))
      
      output$load_output_html <- renderUI({
        if (input$scan_export_engine == "gt")
          import_file() |> gt::gt() |> gt::as_raw_html() |> HTML()
        else
          import_file() |> knitr::kable(format = "html") |> HTML()
        
      })
    }
    
    if (inherits(import_file(), "scdf")) {
      output$load_output_html <- renderUI({
        render_summary()
      })
    }
  }
  
  
  observeEvent(input$scdf_csv, load_file())
  
  observeEvent(input$upload, load_file())

  observeEvent(input$scdf_import, {
    
    if (!inherits(import_file(), "scdf") && 
        !inherits(import_file(), "data.frame")) {
      msg <- "Sorry,\n the file you tried to load is not valid."
      output$load_messages <- renderPrint(msg)
      showNotification(msg, type = "message")
      return(NULL)
    } 
    
    if (inherits(import_file(), "data.frame")) {
      tryCatch(
        read_scdf(
          import_file(),
          cvar = input$scdf_load_cvar,
          pvar = input$scdf_load_pvar,
          dvar = input$scdf_load_dvar,
          mvar = input$scdf_load_mvar,
          na =  eval(str2lang(paste0("c(", input$scdf_load_na, ")")))
        ) |> import_file(),
        error = function(e) {
          msg <- "Sorry,\n the file you tried to load is not a valid scdf file."
          #showNotification(msg, type = "error")
          output$load_messages <- renderPrint({msg; "\n\n"; e})
        }
      )
    }
    
    my_scdf(import_file())
    scdf_render()
    msg <- "loaded file successfully"
    output$load_messages <- renderPrint(cat(msg))
    showNotification(msg, type = "message")
    
    output$load_output_html <- renderUI({
      render_summary()
    })
    output$scdf_messages <- renderPrint(cat(""))
 
  })
  
  
  ## download (save) ----
  output$scdf_save <- downloadHandler(
    filename = function() {
      scdf <- my_scdf()
      out <- paste(
        input$scdf_save_prefix,
        sprintf("%02d", length(scdf)),
        paste0(unique(scdf[[1]]$phase), collapse = ""),
        format(Sys.time(), format = "%y%m%d-%H%M%S"),
        sep = "-"
      )
      paste0(out, input$scdf_save_format)
    },
    
    content = function(file) {
      scdf <- my_scdf()
      
      if (!inherits(scdf, "scdf")) {
        output$scdf_messages <- renderPrint(cat(res$error_msg$scdf_save))
        showNotification(res$error_msg$scdf_save, type = "message")
      } else {
        msg <- "Saved file"
        output$scdf_messages <- renderPrint(cat(msg))
        res$error_msg$scdf_save
      }
      
      if (input$scdf_save_format == ".rds") 
        saveRDS(scdf, file)
      if (input$scdf_save_format == ".R") 
        convert(scdf, file = file)
      if (input$scdf_save_format == ".csv") 
        write_scdf(scdf, filename = file)

    }
  )
  
  ## select case ----
  
  observeEvent(input$new_select_case, {
    case <- input$new_select_case

    if (!identical(case, res$new_case)) {
      casenames <- correct_casenames(my_scdf(), string = TRUE)
      id <- match(case, c(res$new_case, casenames))

      scdf <- my_scdf()[id - 1]
      dv <- scan:::dv(scdf)
      mt <- scan:::mt(scdf)
      phase <- scan:::phase(scdf)
     
      # values string
      x <- split(
        scdf[[1]][[dv]], 
        scdf[[1]][[phase]]
      )
      x <- mapply(
        function(x, n) {
          paste0(n, " = ", paste0(x, collapse = ", "), collapse = "")
        },
        x, names(x)
      )
      values_string <- paste0(x,collapse = "\n")
      updateTextAreaInput(session, "new_values", value = values_string)
      
      # mt string
      imt <- scdf[[1]][[mt]]
      if (identical(as.integer(imt), seq_len(length(imt)))) {
        mt_string <- ""
      } else {
        mt_string <- paste0(imt, collapse = ", ") 
      }
      updateTextInput(session, "new_mt", value = mt_string)
      
      # additional var string
      names_add_var <- names(scdf[[1]])[which(!names(scdf[[1]]) %in% c(dv, phase, mt))]
      if (length(names_add_var) > 0) {
        add_var_str <- lapply(scdf[[1]][, names_add_var, drop = FALSE], function(x) paste0(x, collapse = ", "))
        add_var_str <- paste0(names(add_var_str), " = ", add_var_str, collapse = "\n")
        updateTextAreaInput(session, "new_variables", value = add_var_str)
      }
      
      # name str
      name_str <- names(my_scdf())[id - 1]
      if(!is.null(name_str) && !identical(name_str, "") && !is.na(name_str))
        updateTextInput(session, "new_casename", value = name_str)
    }
  })
  
  ## save case --------
  observeEvent(input$new_save_case, {
    tryCatch({
      values <- paste0("c(", trim(input$new_values), ")")
      dvar <- "values"
      if (inherits(my_scdf(), "scdf")) {
        dvar <- scdf_attr(my_scdf(), "var.values")
      }
      
      call <- paste0(dvar, " = ", values)
      call <- c(call, paste0("dvar = ", deparse(dvar)))
      if (input$new_mt != "") call <- c(call, paste0("mt = c(", input$new_mt, ")"))
      
      if (input$new_variables != "") {
        variables <- input$new_variables |>
          strsplit("\n")  |>
          unlist() |>
          lapply(function(y) strsplit(y, "=")) |>
          unlist(recursive = FALSE) |>
          lapply(function(y) paste0(y[1], " =c(", y[2], ")")) |>
          unlist()
        call <- c(call, variables)
      }
      
      if (input$new_casename != "") {
        call <- c(call, paste0("name = ", deparse(input$new_casename)))
      } else {
        call <- c(call, paste0("name = ", deparse(sample_names())))
      }
      
      call <- paste0(call, collapse = ",")
      call <- paste0("scdf(", call, ")")
      
      new <- call |> str2lang() |> eval()
      
      scdf <- my_scdf()
      casenames <- correct_casenames(scdf, string = TRUE)
      
      case <- input$new_select_case
      
      position <- 0
      if (length(which(casenames %in% case) > 0)) {
        position <- which(casenames %in% case)
      }
     
      if (position == 0) {
        if (length(scdf) > 0) new <- c(scdf, new)
        msg <- "Appended case"
      } else {
        scdf[[position]] <- new[[1]]
        if (is.null(names(new))) names(new) <- ""
        names(scdf)[position] <- names(new)
        new <- scdf
        msg <- paste("Replaced case", case)
      }
      
      output$scdf_messages <- renderPrint(cat(msg))
      showNotification(msg, type = "message")
      
      my_scdf(new)
      scdf_render()
    },
    error = function(e) {
      msg <- paste0(res$error_msg$invalid_case, "\n\n", e)
      output$scdf_messages <- renderPrint(cat(msg))
      #showNotification(msg, type = "error")
    }
      
    )
  })
  
  ## remove cases --------
  observeEvent(input$new_remove_case, {
    
    casenames <- correct_casenames(my_scdf(), string = TRUE)
    active_case <- input$new_select_case
    
    position <- 0
    if (length(which(casenames %in% active_case) > 0)) {
      position <- which(casenames %in% active_case)
    }
    
    if (position > 0) {
      my_scdf(my_scdf()[-position])
      msg <- paste("removed case", casenames[position])
    } else {
      msg <- paste("No case removed. please selct case first.")
    }
    
    output$scdf_messages <- renderPrint(cat(msg))
    showNotification(msg, type = "message")
    
    scdf_render()
  })

  ## clear cases --------
  observeEvent(input$new_clear_cases, {
    my_scdf(NULL)
    
    msg <- "Cleared cases"
    output$scdf_messages <- renderPrint(cat(msg))
    showNotification(msg, type = "message")

    scdf_render()
  })

  # Transform ----

  ## render ----
  transformed <- reactive({
    out <- my_scdf()
    syntax = "scdf"
    if (input$select_cases != "") {
      call <- str2lang(paste0("select_cases(out, ", input$select_cases,")"))
      out <- eval(call)
      syntax <- c(syntax, paste0("select_cases(", input$select_cases,")"))
    }

    if (input$select_phasesA != "" || input$select_phasesB != "") {
 
      out <- paste0(
          "select_phases(out, A = c(", input$select_phasesA, "), B = c(",
          input$select_phasesB, "))"
        ) |>
        str2lang() |> 
        eval()
      
      syntax <- c(
        syntax, 
        paste0(
          "select_phases(A = c(", input$select_phasesA, "), B = c(",
          input$select_phasesB, "))"
        )
      )
    }

    if (input$subset != "") {
      args <-  list(str2lang(input$subset))
      out <- do.call("subset", c(list(out),args))
      syntax <- c(syntax, paste0("subset(",  input$subset, ")"))
    }

    if (input$transform != "") {
      arg <- paste0("transform(out,", trim(input$transform),")")
      out <- str2lang(arg) |> eval()
      syntax <- c(
        syntax, paste0("transform(", gsub("\n", ", ", trim(input$transform)),")")
      )
    }

    if (input$setdvar != "") {
      args <-  list(input$setdvar)
      out <- do.call("set_dvar", c(list(out),args))
      syntax <- c(syntax, paste0("set_dvar(",  deparse(input$setdvar), ")"))
    }

    if (length(syntax)>1) {
      syntax <- syntax[-1]
      syntax <- paste0(
        "scdf", res$pipe_br, " ",
        paste0(syntax, collapse = paste0(res$pipe_br, " "))
      )

    }

    output$transform_syntax <- renderPrint(cat(syntax))

    out
  })

  ## save ----
  output$transformed_save <- downloadHandler(
    filename = function() {
      scdf <- transformed()
      out <- paste(
        input$transform_save_prefix,
        sprintf("%02d", length(scdf)),
        paste0(unique(scdf[[1]]$phase), collapse = ""),
        format(Sys.time(), format = "%y%m%d-%H%M%S"),
        sep = "-"
      )
      paste0(out, input$transform_save_format)
    },
    content = function(file) {
      if (input$transform_save_format == ".rds") 
        saveRDS(transformed(), file)
      if (input$transform_save_format == ".R") 
        convert(transformed(), file = file)
      if (input$transform_save_format == ".csv") 
        write_scdf(transformed(), filename = file)
    }
  )
  
  ## output ------
  
  output$transform_scdf <- renderPrint({
    if(!inherits(my_scdf(), "scdf")) validate(res$msg$no_case)
    print(transformed(), rows = 100)
  })

  output$transform_html <- renderUI({
    if(!inherits(my_scdf(), "scdf")) validate(res$msg$no_case)
    
    options(scan.export.engine = input$scan_export_engine)
    
    if (getOption("scan.export.engine") == "gt")
      export(transformed(), caption = "") |> gt::as_raw_html() |> HTML()
    else
      export(transformed(), caption = "") |> HTML()
    
  })
  
  # Stats -----
  
  stats_class <- reactiveVal()
  
  ## Calculate ---- 
  calculate_stats <- reactive({
    if (!inherits(my_scdf(), "scdf")) validate(res$msg$no_case)
    scdf <- transformed()
    
    if (input$stats_select_case != "all") {
      scdf <- str2lang(paste0("select_cases(scdf, ", input$stats_select_case, ")")) |> eval()
    }
    
    call <- paste0("scan::", get_stats_call())
    
    #if (input$stats_batch) {
    #  call <- paste0("batch_apply(scdf,", call, ")")
    #}
    
    tryCatch(
      out <- str2lang(call) |> eval(),
      error = function(e) {
        msg <- paste0("Sorry, could not proceed with the calculation:\n\n", e)
        #showNotification(msg, type = "error")
        validate(msg)
      }
    )
    # if (input$stats_batch) {
    #   stats_class(class(out[[1]]))
    # } else {
    #   stats_class(class(out))
    # }
    stats_class(class(out))
    update_print()
    out
  })

  update_print <-  reactive({
     stats_class <- stats_class()
     if (is.null(stats_class)) return(NULL)
     out_func <- if (input$stats_out) "export" else "print" 
     call <- paste0("formals(scan:::", out_func, ".", stats_class, ")")
     tryCatch(
       formals <- str2lang(call) |> eval(),
       error = function(e) {
         #showNotification(res$error_msg$html_output, type = "error")
         validate(res$error_msg$html_output)
         return(NULL)
       }
     )
     filter <- lapply(formals, function(x) !is.symbol(x)) |> unlist()
     formals <- formals[filter]
     formals <- lapply(formals, function(x) {
       if (is.character(x)) x <- paste0(paste0("\"", x, "\""))
       x
     })
     id <- which(!names(formals) %in% c("select", "caption", "footnote", "filename"))
     formals <- formals[id]
     if (length(formals) == 0) {
       placeholder <- ""
     } else {
       placeholder <- paste0(names(formals), " = ", formals, collapse = ", ")
     }
     updateTextInput(
       session, inputId = "stats_print_arguments", value = placeholder
     )
    
  })
 
  ## Output ----
  output$stats_html <- renderUI({
    results <- calculate_stats()
    print_args <- input$stats_print_arguments
    options(scan.export.engine = input$scan_export_engine)
    if (!identical(print_args, "")) {
      print_args <- paste0(", ", print_args)
      call<- paste0("export(results, ", print_args, ")")
    } else {
      call <- paste0("export(results)")
    }
    
    # if (input$stats_batch) {
    #   
    #   str_render <- if (getOption("scan.export.engine") == "gt") {
    #     " |> gt::as_raw_html() |> HTML()"
    #   } else {
    #     " |> print() |> as.character() |> HTML()"
    #   }
    #   call <- paste0(
    #     "lapply(results, function(results) {",
    #     call, str_render, "})"
    #   )
    #   
    #   tryCatch(
    #     tmp <- str2lang(call) |> eval(),
    #     error = function(e) {
    #       validate(res$error_msg$html_output, e)
    #     }
    #   )  
    #   
    # } else {
    tryCatch(
      if (getOption("scan.export.engine") == "gt") {
        str2lang(call) |> eval() |> gt::as_raw_html() |> HTML()
      } else {
        str2lang(call) |> eval() |> print() |> as.character() |> HTML()
      },
      error = function(e) {
        validate(res$error_msg$html_output)
      }
    )    
    
    

  })

  output$stats_text <- renderPrint({
    results <- calculate_stats()
   
    print_args <- input$stats_print_arguments
    if (print_args != "") {
      print_args <- paste0(", ", print_args)
      call<- paste0("print(results, ", print_args, ")")
    } else call <- "print(results)"
    
    # if (input$stats_batch) {
    #  
    #   call <- paste0(
    #     "mapply(function(results, nm) {",
    #     "cat('\\nCase:', nm, '\\n\\n'); ",
    #     call, 
    #     "; cat('\\n", strrep("\\u2501", 75), "\\n')",
    #     "}, results, nm = names(results))"
    #   )
    #  
    #   tmp <- str2lang(call) |> eval()
    #   
    # } else {
    #   str2lang(call) |> eval()      
    # }
    str2lang(call) |> eval()     

  })

  
  observeEvent(input$func, {
    if(input$stats_description) {
    
      hf <- help(input$func, package = "scan")
      res <- utils:::.getHelpFile(hf)
      desc <- Filter(function(x) attr(x, "Rd_tag") == "\\description", res)
      if (length(desc) > 0) {
        desc <- paste0(unlist(desc)[-1], collapse = "")
        desc <- gsub("\n", "<br>", trimws(desc))
        output$stats_description <- renderUI({
          tags$p(HTML(desc), style = "font-size:18px; color:darkblue;")
        })

      }
    }
    updateTextAreaInput(session, "stats_print_arguments", value = "")
  })
  
  output$stats_syntax <- renderPrint({
    cat(get_stats_call())
  })


  ## Arguments ------
  stat_arg_names <- reactive({
    .formals <- formals(eval(str2lang(paste0("scan::", input$func))))
    args <- names(.formals)
    values <- .formals

    id <- which(!args %in% c(
      "dvar", "pvar", "mvar", "phases",
      "data", "scdf", "data.l2", "offset", "lag.max",
      "graph", "output", "...")
    )
    if (input$func == "mplm") {
      id <- c(which(args == "dvar") , id)
      values[which(args == "dvar")] <- scan:::dv(transformed())
    }
    
    list(names = args[id], values = values[id])
  })

  output$stats_arguments <- renderUI({
    args <- stat_arg_names()

    out <- vector("list", length(args$names))
    if (length(out) > 0) {
      for (i in 1:length(out)) {

        value <- args$values[[i]]
        if (is.character(value)) value <- deparse(value)
   
        if (!is.name(value) && !inherits(value, "call") && isTRUE(is.na(value))) 
          value <- substitute(value) |> deparse()
        if (is.null(value)) value <- substitute(value) |> deparse()
        if (!is.numeric(value) && !is.logical(value) && !is.character(value) &&
            !is.call(value)) {
          value <- substitute(value) |> deparse()
        }
        if (is.call(value)) {
          if (is.character(eval(value))) {
            value <- eval(value)
          } else {
            value <- substitute(value) |> deparse()
          }
        }
        outvalue <- value

        if (length(value) > 1) {
          choices <- setNames(quoted(value), value)
          selected <- names(choices)[1]
          out[[i]] <- selectInput(
            args$names[i], args$names[i],
            choices = choices,
            selected = selected
          )
        } else if (is.numeric(value)) {
          out[[i]] <- numericInput(
            args$names[i], args$names[i], value = outvalue
          )
        } else if (is.logical(value)) {
          choices <- c("FALSE" = FALSE, "TRUE" = TRUE)
          #out[[i]] <- div(
          #  style = "display: flex; align-items: center; vertical-align: top; padding-left: 5px;",
          #  tags$label(args$names[i]),
          #  radioButtons(args$names[i], NULL,
          #               choices = choices,
          #               inline = TRUE)
          #)
          out[[i]] <- checkboxInput( 
            args$names[i], 
            tags$span(args$names[i], class = "chklabel-big"),
            value = outvalue
          )
          #out[[i]] <- radioButtons(
          #  args$names[i], args$names[i],
          #  choices = choices,
          #  inline = TRUE, selected = outvalue
          #)
        } else {
          out[[i]] <- textInput(args$names[i], args$names[i], value = outvalue)
        }
      }
      return(out)
    }
  })

  ## get stats call ----
  get_stats_call <- reactive({
    full_args <- stat_arg_names()
    args <- full_args
    values <- sapply(args$names, function(name) input[[name]])
    args <- args$names
    options(scan.rename.predictors = input$rename_predictors)
    
    if (!input$stats_default) {
      id_default <- mapply(
        function(origin, new) {
  
          if (typeof(origin) == "language") origin <- eval(origin)
          if (typeof(origin) == "symbol") origin <- deparse(origin)
          if (identical(new, origin)) return(TRUE)
          if (identical(new, as.character(origin[1]))) return(TRUE)
          if (identical(new, deparse(origin))) return(TRUE)
          
          if (substr(deparse(origin[1]), 1,3) == "NA_") origin <- NA
          if (identical(new, deparse(origin[1]))) return(TRUE)
    
          return(FALSE)
        },
        origin = full_args$values,
        new = values
      ) |> unlist()
      values[id_default] <- ""
    }
    
    id <- which(values != "")

    args <- args[id]
    values <- values[id]

    #str_scdf <- if (input$stats_batch) "." else "scdf"
    str_scdf <- "scdf"
    call <- paste0(
      input$func, "(", str_scdf,
      if (length(args > 0)) {
        paste0(", ",paste0(args, " = ", values, collapse = ", "))
      } else {
        ""
      },
      ")"
    )
    call
  })

  ## Save ------
  
  condition_for_output <- reactive({
    TRUE#!(input$stats_out && input$stats_batch)
  })
  
  
  output$stats_save <- downloadHandler(
    
    filename = function() {
      scdf <- transformed()
      out <- paste(
        input$prefix_output_stats, input$func,
        sprintf("%02d", length(scdf)),
        paste0(unique(scdf[[1]]$phase), collapse = ""),
        format(Sys.time(), format = "%y%m%d-%H%M%S"),
        sep = "-"
      )
      ext <- if (!input$stats_out) "txt" else "html"
      if (input$setting_output_docx && input$stats_out) ext <- "docx" 
      out <- paste0(out, ".", ext)
      out
    },
    content = function(file) {
      
      if (!(condition_for_output())) {
        showNotification(
          "An html or docx output for case-by-case analyses is not avaliable yet. Please switch to the text output before saving.", 
          duration = 10,
          type = "error")
        return(invisible(NULL)) 
      }
      
      if (!input$stats_out) {
        results <- calculate_stats()
        print_args <- input$stats_print_arguments
        if (print_args != "") {
          print_args <- paste0(", ", print_args)
          call<- paste0("print(results, ", print_args, ")")
        } else call <- "print(results)"
        
        # if (input$stats_batch) {
        #   call <- paste0(
        #     "tmp <- mapply(function(results, nm) {",
        #     "cat('\\nCase:', nm, '\\n\\n'); ",
        #     call, 
        #     "; cat('\\n", strrep("\\u2501", 75), "\\n')",
        #     "}, results, nm = names(results))"
        #   )
        # }
        
        call <- paste0("capture.output(", call, ")")
        writeLines(str2lang(call) |> eval(), con = file)
      }
      
      if (input$stats_out) {
        results <- calculate_stats()
        print_args <- input$stats_print_arguments
        if (print_args != "") {
          print_args <- paste0(", ", print_args)
          call<- paste0(
            "export(results, ", print_args, ", filename = '", file, "')" 
          )
        } else {
          call <- paste0("export(results, filename = '", file, "')")
        }
        
        out <- if (getOption("scan.export.engine") == "gt")
          str2lang(call) |> eval() |> gt::as_raw_html()
        else
          str2lang(call) |> eval()
      }
    }
  )
  
  # # Plot -----
  # 
  # ## Render ----
  # render_plot <- reactive({
  #   req(inherits(my_scdf(), "scdf"))
  #   call <- paste0("scplot(transformed())")
  #   if (trimws(input$plot_arguments) != "") {
  #     plot_args <- trimws(input$plot_arguments)
  #     plot_args <- gsub("\n+", "\n", plot_args)
  #     call <- paste0(
  #       call, res$pipe, gsub("\n", res$pipe, plot_args)
  #     )
  #   }
  #   call <- paste0("print(",call,")")
  #   tryCatch(
  #     str2lang(call) |> eval(),
  #     error = function(x) {
  #       msg <- paste0(res$error_msg$plot, "\n\n", x)
  #       output$plot_syntax <- renderPrint(cat(msg))
  #       #showNotification(msg, type = "error")
  #     }
  #   )
  # })
  # 
  # observeEvent(input$scplot_templates_design, {
  #   new_value <- unname(
  #     res$choices$scplot_templates_design[input$scplot_templates_design]
  #   )
  #   old_value <- input$plot_arguments
  #   if (old_value == "") {
  #     value <- new_value
  #   } else {
  #     value <- paste0(input$plot_arguments, "\n", new_value)
  #   }
  # updateTextAreaInput(inputId = "plot_arguments", value = value)
  # })
  # 
  # observeEvent(input$scplot_templates_annotate, {
  #   new_value <- unname(
  #     res$choices$scplot_templates_annotate[input$scplot_templates_annotate]
  #   )
  #   old_value <- input$plot_arguments
  #   if (old_value == "") {
  #     value <- new_value
  #   } else {
  #     value <- paste0(input$plot_arguments, "\n", new_value)
  #   }
  #   updateTextAreaInput(inputId = "plot_arguments", value = value)
  # })
  # 
  # observeEvent(input$scplot_examples, {
  #   if ("(empty selection)" == input$scplot_examples) {
  #     value <- ""
  #   } else {
  #     new_value <- unname(res$choices$scplot_examples[input$scplot_examples])
  #     old_value <- input$plot_arguments
  #     if (old_value == "") {
  #       value <- new_value
  #     } else {
  #       value <- paste0(input$plot_arguments, "\n", new_value)
  #     }
  #   }
  #   updateTextAreaInput(inputId = "plot_arguments", value = value)
  # })
  # 
  # observeEvent(input$plot_arguments, render_plot_syntax())
  # 
  # ## Output ----
  # 
  # render_plot_syntax <- reactive({
  #   call <- paste0("scplot(scdf)")
  #   if (trimws(input$plot_arguments) != "") {
  #     call <- paste0(
  #       call, res$pipe_br, " ", gsub("\n", paste0(res$pipe_br, " "), trimws(input$plot_arguments))
  #     )
  #   }
  #   output$plot_syntax <- renderPrint({
  #     cat(call)
  #   })
  # })
  # 
  # output$plot_scdf <- renderPlot(res = 120, {
  #   render_plot()
  # })
  # 
  # ## Save ----
  # output$saveplot <- downloadHandler(
  #   filename = function() {
  #     scdf <- transformed()
  #     out <- paste(
  #       input$prefix_output_plot,
  #       sprintf("%02d", length(scdf)),
  #       paste0(unique(scdf[[1]]$phase), collapse = ""),
  #       format(Sys.time(), format = "%y%m%d-%H%M%S"),
  #       sep = "-"
  #     )
  #     paste0(out, ".png")
  #   },
  #   content = function(file) {
  #     ggplot2::ggsave(
  #       file, render_plot(), width = input$width, height = input$height,
  #       dpi = input$dpi, units = "px",  device = "png"
  #     )
  #   }
  # )
  # 
  # 
  
  
  # Power test -----
  
  output$pt_results <- renderPrint(cat(res$placeholder$pt))
  
  ## Render -----
  render_power_test <- reactive({
    syntax <- paste0(
      "design(\n  n = ", input$design_n, ", ",
      "phase_design = list(", input$design_phase, "), \n  ",
      "trend = ", input$design_trend, ", ",
      "level = list(", input$design_level, "), ",
      "slope = list(", input$design_slope, "), \n  ",
      "start_value = ", input$design_start, ", ",
      #"s = ", input$design_s, ", ",
      "rtt = ", input$design_rtt, ", ",
      "distribution = ", deparse(input$design_distribution),
      "\n)"
    )  
    
    ci <- input$pt_ci
    
    pt_method <- deparse(as.list(input$pt_method))
    if (input$pt_method_user != "") {
      
      if (length(input$pt_method) >0) {
        pt_method <- paste0("\"", input$pt_method, "\"",collapse = ",")
        pt_method <- paste0(
          "list(user = function(scdf) {", input$pt_method_user,"}, ",
          pt_method, ")"
        )
      } else {
        pt_method <- paste0("list(user = function(scdf) {", input$pt_method_user,"})")
      }
    }
    
    syntax <- paste0(
      syntax, res$pipe,
      "  power_test(method = ", pt_method, ", ",
      "effect = ", deparse(input$pt_effect), ", ",
      "n_sim = ", input$pt_n, 
      if (!identical(ci, "")) paste0(", ci = ", ci),
      ")"
    )
    
    syntax
  })
  
  ## Output ----
  output$pt_syntax <- renderPrint({
    cat(render_power_test())
  })
  
  ## Plot ----
  
  observeEvent(input$desigh_plot_button, {
    call <- paste0(
      "design(\n  n = ", input$design_n, ", ",
      "phase_design = list(", input$design_phase, "), \n  ",
      "trend = ", input$design_trend, ", ",
      "level = list(", input$design_level, "), ",
      "slope = list(", input$design_slope, "), \n  ",
      "start_value = ", input$design_start, ", ",
      #"s = ", input$design_s, ", ",
      "rtt = ", input$design_rtt, ", ",
      "distribution = ", deparse(input$design_distribution),
      "\n)", res$pipe,
      "random_scdf()", res$pipe,
      "scplot()"
    )
    
    output$plot_design <- renderPlot(res = 100, {
      str2lang(call) |> eval()
    })
  })
  
  ## Analyse ----
  
  observeEvent(input$pt_compute, {
    
    phase_structure <- eval(str2lang(
      paste0("list(", input$design_phase, ")")
    ))
    if (length(phase_structure) > 2) {
      output$pt_results <- renderPrint({
        cat("Sorry, power-tests are only possible for designs with two phases.")
      })
    } else {
      call <- render_power_test()
      output$pt_results <- renderPrint({cat("Calculating ...")})
      res <- tryCatch(
        str2lang(call) |> eval(),
        error = function(e) {
          msg <- paste0("Sorry, could not proceed calculation:\n\n", e)
          #showNotification(msg, type = "error")
          msg
        }
          
      )
      output$pt_results <- renderPrint({
        if (inherits(res, "character")) cat(res) else res
      })
    }
    
  })
  
  # quit app -----
  observeEvent(input$navpage, {
    if (input$navpage == "Quit") {
      session$sendCustomMessage(
        type = "closeWindow", 
        list(message = "window.close();")
      )
      stopApp()
    }
  })
  
  # Plot new -----
  
  create_scplot_call <- reactive({
    themes <- c(input$scplot_theme_1, input$scplot_theme_2, input$scplot_theme_3)
    themes <- themes[which(themes != "None")]
    
    
    
    call <- c(
      "transformed()",
      if (input$scplot_select_case != "all") {
        paste0("select_cases(", input$scplot_select_case, ")")
      },
      "scplot()",
      if (!is.na(input$scplot_ymin) || !is.na(input$scplot_ymax)) {
        paste0("set_yaxis(limits = c(", input$scplot_ymin, ",", input$scplot_ymax, "))")
      },
      
      if (input$scplot_stats_mean_a) 'add_statline("mean", phase = "A")',
      if (input$scplot_stats_median_a) 'add_statline("median", phase = "A")',
      if (input$scplot_stats_max_a) 'add_statline("max", phase = "A")',
      if (input$scplot_stats_min_a) 'add_statline("min", phase = "A")',
      if (input$scplot_stats_trend) 'add_statline("trend")',
      if (input$scplot_stats_mean) 'add_statline("mean")',
      if (input$scplot_stats_median) 'add_statline("median")',
      if (input$scplot_stats_moving) 'add_statline("moving mean")',
      if (input$scplot_stats_loess) 'add_statline("loess", span = 0.4)',
      if (input$scplot_stats_trend_a) 'add_statline("trendA")',
      if (input$scplot_add != "None") paste0('set_dataline("', input$scplot_add, '")'),
      
      paste0("set_theme(", paste0("'", themes, "'", collapse = ", "), ")"),
      if (input$scplot_text_size > 6) paste0('set_base_text(size = ', input$scplot_text_size, ')'),
      if (input$scplot_legend) 'add_legend()'
      
    )
    call <- paste0(call, collapse = " |>\n\t")
    call
  })
  
  
  
  
  output$scplot_syntax <- renderPrint({

    #call <- create_scplot_call()
    #cat(call)
  })
  
  ## Render plot new----

  output$scplot_plot <- renderPlot(res = 120, {
    req(inherits(my_scdf(), "scdf"))
    
    call <- paste0(create_scplot_call(), " |> print()")
    
    tryCatch(
      out <- str2lang(call) |> eval(),
      error = function(x) {
        msg <- paste0(res$error_msg$plot, "\n\n", x)
        out <- renderPrint(cat(msg))
        
      }
    )
    out
  })
  
  ## Output Render plot new----
  

  ## Save Render plot new----
  output$saveplot_2 <- downloadHandler(
    filename = function() {
      scdf <- transformed()
      out <- paste(
        input$prefix_output_plot,
        sprintf("%02d", length(scdf)),
        paste0(unique(scdf[[1]]$phase), collapse = ""),
        format(Sys.time(), format = "%y%m%d-%H%M%S"),
        sep = "-"
      )
      paste0(out, ".png")
    },
    content = function(file) {
      ggplot2::ggsave(
        file, 
        paste0(create_scplot_call(), " |> print()") |> str2lang() |> eval(),
        width = input$width, height = input$height,
        dpi = input$dpi, units = "px",  device = "png"
      )
    }
  )
  
  
}
