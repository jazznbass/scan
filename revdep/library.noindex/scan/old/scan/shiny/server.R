
server <- function(input, output, session) {

  # scdf ----
  
  ## startup message ----
  
  output$scdf_messages <- renderPrint(cat(res$msg$startup))

  ## Render ----
  my_scdf <- reactiveVal()
  
  scdf_render <- reactive({
    
    if (input$scdf_output_format == "Summary"){
      output$scdf_output <- renderPrint({
        if (!inherits(my_scdf(), "scdf")) validate(res$msg$no_case)
        if (identical(length(my_scdf()), 0)) validate(res$msg$no_case)
        do.call("summary", list(my_scdf()))
      })
    } else if (input$scdf_output_format == "Syntax") {
      output$scdf_output <- renderPrint({
        if (!inherits(my_scdf(), "scdf")) validate(res$msg$no_case)
        if (identical(length(my_scdf()), 0)) validate(res$msg$no_case)
        do.call("convert", list(
          my_scdf(), inline = as.logical(input$scdf_syntax_phase_structure)
        ))
      })
    }

  })

  observeEvent(input$scdf_output_format, scdf_render()) 
  
  ## input example ----
  observeEvent(input$scdf_example, {
    if (input$scdf_example != "(none)") {
      my_scdf(paste0("scan::", input$scdf_example) |> str2lang() |> eval())
      scdf_render()
      output$load_messages <- renderPrint(
        cat(paste0("loaded example ", input$scdf_example))
      )
      output$load_output <- renderPrint({
         do.call("summary", list(my_scdf()))
      })
      #output$load_html_output <- renderUI({
      #   x <- do.call("summary", list(my_scdf()))
      #   export(x) |> HTML()
      #})
      output$scdf_messages <- renderPrint(cat(""))
    } else {
      my_scdf(NULL)
    }
  })

  ## upload (load) ------
  observeEvent(input$upload, {
    ext <- tools::file_ext(input$upload$datapath)
    if (ext == "rds") {
      new <- readRDS(input$upload$datapath)
    } else if (ext %in% c("r", "R")) {
      new <- readLines(input$upload$datapath)
      new <- paste0(new, collapse = "\n")
      .tmp <- new.env()
      eval(parse(text = new), envir = .tmp)
      new <- .tmp$study
    } else {
      na <- eval(str2lang(paste0("c(", input$scdf_load_na, ")")))
      new <- read_scdf(input$upload$datapath, na = na)
    }

    if (!inherits(new, "scdf")) {
      output$load_messages <- renderText(
        "Sorry,\n the file you tried to load is not a valid scdf file.")
    } else {
      my_scdf(new)
      scdf_render()
      output$load_messages <- renderPrint(cat(paste0("loaded file successfully")))
      output$load_output <- renderPrint({
        do.call("summary", list(my_scdf()))
      })
      output$scdf_messages <- renderPrint(cat(""))
    }

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
      } else {
        output$scdf_messages <- renderPrint(cat("Saved file"))
      }
      
      if (input$scdf_save_format == ".rds") 
        saveRDS(scdf, file)
      if (input$scdf_save_format == ".R") 
        convert(scdf, file = file)
      if (input$scdf_save_format == ".csv") 
        write_scdf(scdf, filename = file)

    }
  )

  ## new cases --------
  observeEvent(input$add_case, {
    tryCatch({
      values <- paste0("c(", trim(input$values), ")")
      dvar <- "values"
      if (inherits(my_scdf(), "scdf")) {
        dvar <- scdf_attr(my_scdf(), "var.values")
      }

      call <- paste0(dvar, " = ", values)
      call <- c(call, paste0("dvar = ", deparse(dvar)))
      if (input$mt != "") call <- c(call, paste0("mt = c(", input$mt, ")"))
      
      if (input$variables != "") {
        variables <- input$variables |>
          strsplit("\n")  |>
          unlist() |>
          lapply(function(y) strsplit(y, "=")) |>
          unlist(recursive = FALSE) |>
          lapply(function(y) paste0(y[1], " =c(", y[2], ")")) |>
          unlist()
        call <- c(call, variables)
      }

      if (input$casename != "") {
        call <- c(call, paste0("name = ", deparse(input$casename)))
      } else {
        call <- c(call, paste0("name = \"case\""))
      }
      call <- paste0(call, collapse = ",")
      call <- paste0("scdf(", call, ")")

      new <- call |> str2lang() |> eval()
      if (input$remove_which == "last") {
        if (length(my_scdf()) > 0) new <- c(my_scdf(), new)
        my_scdf(new)
        output$scdf_messages <- renderPrint(cat("Appended case"))
        scdf_render()
      } 
      
      if (input$remove_which == "at") {
        at <- input$remove_at
        if (length(my_scdf()) >= at - 1) {
          if (at == 1) {
            new <- c(new, my_scdf())
          } else if (at == length(my_scdf()) + 1) {
            new <- c(my_scdf(), new)
          } else {
            new <- c(my_scdf()[1:(at-1)], new, my_scdf()[at:(length(my_scdf()))])
          }
          my_scdf(new)
          output$scdf_messages <- renderPrint(cat("Added case at position", input$remove_at))
          scdf_render() 
        }
      }  

    },
    error = function(e)
      output$scdf_messages <- renderText(
        paste0(res$error_msg$invalid_case, "\n\n", e)
      )
    )
  })

  ## remove cases --------
  observeEvent(input$remove_case, {
    if (input$remove_which == "last") {
      if (length(my_scdf()) > 1) {
        my_scdf(my_scdf()[-length(my_scdf())])
      } else (my_scdf(NULL))
    }
    
    if (input$remove_which == "at") {
      at <- input$remove_at
      if (length(my_scdf()) >= at)
        my_scdf(my_scdf()[-input$remove_at])
    }
    
    output$scdf_messages <- renderPrint(cat("removed case"))
    scdf_render()
  })

  ## clear cases --------
  observeEvent(input$clear_cases, {
    my_scdf(NULL)
    output$scdf_messages <- renderPrint(cat("Cleared cases"))
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
      syntax <- c(syntax, paste0("select_cases(",input$select_cases,")"))
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

  ## Calculate ---- 
  calculate_stats <- reactive({
    if (!inherits(my_scdf(), "scdf")) validate(res$msg$no_case)
    scdf <- transformed()
    call <- paste0("scan::", get_stats_call())
    tryCatch(
      str2lang(call) |> eval(),
      error = function(e)
        validate(paste0("Sorry, could not proceed calculation:\n\n", e))
    )
  })

  ## Output ----
  output$stats_html <- renderUI({
    results <- calculate_stats()
    print_args <- input$stats_print_arguments
    flip <- paste0("flip = ", input$stats_export_flip)
    options(scan.export.engine = input$scan_export_engine)
    if (print_args != "") {
      print_args <- paste0(", ", print_args)
      call<- paste0("export(results, ", flip, ", ", print_args, ")")
    } else call <- paste0("export(results,", flip ,")")
    tryCatch(
      if (getOption("scan.export.engine") == "gt")
        str2lang(call) |> eval() |> gt::as_raw_html() |> HTML()
      else
        str2lang(call) |> eval() |> print() |> as.character() |> HTML(),
      error = function(e)
        validate(paste0("Sorry, no html export for this function available yet.", "\n",e))
    )
  })

  output$stats_text <- renderPrint({
    results <- calculate_stats()

    print_args <- input$stats_print_arguments
    if (print_args != "") {
      print_args <- paste0(", ", print_args)
      call<- paste0("print(results, ", print_args, ")")
    } else call <- "print(results)"

    str2lang(call) |> eval()
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
        if (isTRUE(is.na(value))) value <- substitute(value) |> deparse()
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
        #if (input$stats_default == "Yes") outvalue <- value else outvalue = NULL
        outvalue <- value

        if (length(value) > 1) {
          choices <- setNames(quoted(value), value)
          #if (input$stats_default == "No")
          #  choices <- c("(default)" = "", choices)
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
          choices <- c("FALSE", "TRUE")
          #if (input$stats_default == "No")
          #  choices <- c("(default)" = "", choices)
          out[[i]] <- radioButtons(
            args$names[i], args$names[i],
            choices = choices,
            inline = TRUE, selected = outvalue
          )
        } else {
          out[[i]] <- textInput(args$names[i], args$names[i], value = outvalue)
        }
      }
      return(out)
    }
  })

  get_stats_call <- reactive({
    full_args <- stat_arg_names()
    args <- full_args
    values <- sapply(args$names, function(name) input[[name]])
    args <- args$names
    #print(full_args$values)
    #print(values)
    if (input$stats_default == "No") {
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

    call <- paste0(
      input$func, "(scdf",
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
      out <- paste0(out, ".", input$format_output_stats)
      out
    },
    content = function(file) {
      
      if (input$format_output_stats == "text") {
        results <- calculate_stats()
        print_args <- input$stats_print_arguments
        if (print_args != "") {
          print_args <- paste0(", ", print_args)
          call<- paste0("print(results, ", print_args, ")")
        } else call <- "print(results)"
        call <- paste0("capture.output(", call, ")")
        writeLines(str2lang(call) |> eval(), con = file)
      } else {
        results <- calculate_stats()
        print_args <- input$stats_print_arguments
        if (print_args != "") {
          print_args <- paste0(", ", print_args)
          call<- paste0(
            "export(results, flip = ", input$stats_export_flip, ",",
            print_args, 
            ", filename = '", file, "')" 
          )
        } else {
          call <- paste0(
            "export(results, flip = ", input$stats_export_flip,
            ", filename = '", file, "')"
            
          )
        }
        
        out <- if (getOption("scan.export.engine") == "gt")
          str2lang(call) |> eval() |> gt::as_raw_html()
        else
          str2lang(call) |> eval()

      }
      
      #if (input$stats_out == "Html") {
      #  results <- calculate_stats()
      #  print_args <- input$stats_print_arguments
      #  if (print_args != "") {
      #    print_args <- paste0(", ", print_args)
      #    call<- paste0("export(results, ", print_args, ")")
      #  } else {
      #    call <- "export(results)"
      #  }
      #  out <- str2lang(call) |> eval()
      #  kableExtra::save_kable(out, file)
      #}
      
    }
  )
  
  # Plot -----

  ## Render ----
  render_plot <- reactive({
    req(inherits(my_scdf(), "scdf"))
    call <- paste0("scplot(transformed())")
    if (trimws(input$plot_arguments) != "") {
      plot_args <- trimws(input$plot_arguments)
      plot_args <- gsub("\n+", "\n", plot_args)
      call <- paste0(
        call, res$pipe, gsub("\n", res$pipe, plot_args)
      )
    }
    call <- paste0("print(",call,")")
    tryCatch(
      str2lang(call) |> eval(),
      error = function(x)
        output$plot_syntax <- renderText(paste0(res$error_msg$plot, "\n\n", x))
    )
  })

  observeEvent(input$scplot_templates_design, {
    new_value <- unname(
      res$choices$scplot_templates_design[input$scplot_templates_design]
    )
    old_value <- input$plot_arguments
    if (old_value == "") {
      value <- new_value
    } else {
      value <- paste0(input$plot_arguments, "\n", new_value)
    }
  updateTextAreaInput(inputId = "plot_arguments", value = value)
  })

  observeEvent(input$scplot_templates_annotate, {
    new_value <- unname(
      res$choices$scplot_templates_annotate[input$scplot_templates_annotate]
    )
    old_value <- input$plot_arguments
    if (old_value == "") {
      value <- new_value
    } else {
      value <- paste0(input$plot_arguments, "\n", new_value)
    }
    updateTextAreaInput(inputId = "plot_arguments", value = value)
  })
  
  observeEvent(input$scplot_examples, {
    if ("(empty selection)" == input$scplot_examples) {
      value <- ""
    } else {
      new_value <- unname(res$choices$scplot_examples[input$scplot_examples])
      old_value <- input$plot_arguments
      if (old_value == "") {
        value <- new_value
      } else {
        value <- paste0(input$plot_arguments, "\n", new_value)
      }
    }
    updateTextAreaInput(inputId = "plot_arguments", value = value)
  })

  observeEvent(input$plot_arguments, render_plot_syntax())

  ## Output ----
 
  render_plot_syntax <- reactive({
    call <- paste0("scplot(scdf)")
    if (trimws(input$plot_arguments) != "") {
      call <- paste0(
        call, res$pipe_br, " ", gsub("\n", paste0(res$pipe_br, " "), trimws(input$plot_arguments))
      )
    }
    output$plot_syntax <- renderPrint({
      cat(call)
    })
  })
  
  output$plot_scdf <- renderPlot(res = 120,{
    render_plot()
  })

  ## Save ----
  output$saveplot <- downloadHandler(
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
        file, render_plot(), width = input$width, height = input$height,
        dpi = input$dpi, units = "px",  device = "png"
      )
    }
  )

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
    #tryCatch(
    #  str2lang(call) |> eval(),
    #  error = function(x)
    #    output <- renderText(paste0(res$error_msg$plot, "\n\n", x))
    #)

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
        error = function(e)
          paste0("Sorry, could not proceed calculation:\n\n", e)
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
  
}
