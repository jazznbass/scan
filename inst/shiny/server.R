
server <- function(input, output, session) {

  # Startup message
  output$scdf_summary <- renderPrint(cat(res$msg$startup))

  my_scdf <- reactiveVal()

  scdf_render <- reactive({

    if (!inherits(my_scdf(), "scdf")) validate(res$msg$no_case)

    output$scdf_summary <- renderPrint({
      do.call("summary", list(my_scdf()))
    })

    output$scdf_syntax <- renderPrint({
      req(inherits(my_scdf(), "scdf"))
      do.call("convert", list(my_scdf()))
    })

  })

  observeEvent(input$scdf_example, {
    if (input$scdf_example != "(none)") {
      my_scdf(paste0("scan::", input$scdf_example) |> str2lang() |> eval())
      scdf_render()
    } else {
      my_scdf(NULL)
    }
  })


  # scdf: upload / save ------
  observeEvent(input$upload, {
    ext <- tools::file_ext(input$upload$datapath)
    if (ext == "rds") {
      new <- readRDS(input$upload$datapath)
    } else {
      new <- read_scdf(input$upload$datapath)
    }

    if (!inherits(new, "scdf")) {
      output$scdf_summary <- renderText(
        "Sorry,\n the file you tried to upload is not a valid scdf file.")
    } else {
      my_scdf(new)
      scdf_render()
    }

  })

  output$scdf_save <- downloadHandler(
    filename = function() "my_scdf.rds",
    content = function(file) saveRDS(my_scdf(), file)
  )

  # scdf: new cases --------

  observeEvent(input$add_case, {
    tryCatch({
      values <- paste0("c(", trim(input$values), ")")
      dvar <- "values"
      if (inherits(my_scdf(), "scdf")) {
        dvar <- scdf_attr(my_scdf(), "var.values")
      }

      call <- paste0(dvar, " = ", values)
      call <- c(call, paste0("dvar = ", deparse(dvar)))
      if (input$mt != "") call <- c(call, paste0("mt = ", deparse(mt)))

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

      if (input$casename != "")
        call <- c(call, paste0("name = ", deparse(input$casename)))

      call <- paste0(call, collapse = ",")
      call <- paste0("scdf(", call, ")")

      new <- call |> str2lang() |> eval()
      if (length(my_scdf()) > 0) new <- c(my_scdf(), new)
      my_scdf(new)
      scdf_render()
    },
    error = function(e)
      output$scdf_summary <- renderText(
        paste0(res$error_msg$invalid_case, "\n\n", e)
      )
    )
  })

  observeEvent(input$remove_case, {
    if (length(my_scdf()) > 1) {
      my_scdf(my_scdf()[-length(my_scdf())])
    } else (my_scdf(NULL))
    scdf_render()
  })

  observeEvent(input$remove_all, {
    my_scdf(NULL)
    scdf_render()
  })

  # transform ----

  transformed <- reactive({
    out <- my_scdf()
    syntax = "scdf"
    if (input$select_cases != "") {
      args <- list(str2lang(input$select_cases))
      out <- do.call("select_cases", c(list(out), args))
      syntax <- c(syntax, paste0("select_cases(",input$select_cases,")"))

    }

    if (input$select_phases != "") {
      out <- paste0("select_phases(out, ", input$select_phases, ")") |>
        str2lang() |> eval()
      syntax <- c(syntax, paste0("select_phases(", input$select_phases, ")"))
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
        "scdf %>%\n  ",
        paste0(syntax, collapse = " %>%\n  ")
      )

    }

    output$transform_syntax <- renderPrint(cat(syntax))

    out
  })

  output$transform_scdf <- renderPrint({
    if(!inherits(my_scdf(), "scdf")) validate(res$msg$no_case)
    print(transformed(), rows = 100)
  })

  output$transform_save <- downloadHandler(
    filename = function() "my_scdf.rds",
    content = function(file) saveRDS(transformed(), file)
  )

  # stats -----

  calculate_stats <- reactive({
    if (!inherits(my_scdf(), "scdf")) validate(res$msg$no_case)
    scdf <- transformed()
    call <- get_stats_call()
    tryCatch(
      str2lang(call) |> eval(),
      error = function(e)
        validate(paste0("Sorry, could not proceed calculation:\n\n", e))
    )
  })

  output$stats_html <- renderUI({
    results <- calculate_stats()
    print_args <- input$stats_print_arguments
    if (print_args != "") {
      print_args <- paste0(", ", print_args)
      call<- paste0("export(results, ", print_args, ")")
    } else call <- "export(results)"
    tryCatch(
      str2lang(call) |> eval() |> HTML(),
      error = function(e)
        validate("Sorry, no html export for this function available yet.")
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


  observeEvent(input$stats_help, {
    link <- paste0(
      "https://jazznbass.github.io/scan/reference/", input$func, ".html"
    )
    shinyjs::js$openURL(link)
  })

  output$stats_syntax <- renderPrint({
    cat(get_stats_call())
  })


  # stats: arguments ------

  stat_arg_names <- reactive({
    args <- names(formals(input$func))
    values <- formals(input$func)

    id <- which(!args %in% c(
      "dvar", "pvar", "mvar", "phases", "meta_method",
      "data", "scdf", "data.l2", "offset", "lag.max",
      "graph", "output", "...")
    )
    args <- args[id]
    values <- values[id]
    list(names = args, values = values)
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
        if (input$stats_default == "Yes") outvalue <- value else outvalue = NULL

        if (length(value) > 1) {
          choices <- setNames(quoted(value), value)
          if (input$stats_default == "No")
            choices <- c("(default)" = "", choices)
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
          if (input$stats_default == "No")
            choices <- c("(default)" = "", choices)
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
    args <- stat_arg_names()
    values <- sapply(args$names, function(name) input[[name]])
    args <- args$names

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


  # plot -----

  observeEvent(input$plot_help, {
    if (input$plot == "scplot") {
      link <- "https://jazznbass.github.io/scplot/reference/index.html"
    } else if (input$plot == "plot.scdf") {
      link <- "https://jazznbass.github.io/scan/reference/plot.scdf.html"
    }
    shinyjs::js$openURL(link)
  })

  render_plot <- reactive({
    req(inherits(my_scdf(), "scdf"))
    if (input$plot == "scplot") {
      call <- paste0("scplot(transformed())")
      if (trimws(input$plot_arguments) != "") {
        call <- paste0(
          call, "%>% ", gsub("\n", " %>% ", trimws(input$plot_arguments))
        )
      }
      call <- paste0("print(",call,")")
    } else if (input$plot == "plot.scdf") {
      call <- paste0(
        "plot(transformed(), ", trim(input$plot_arguments), ")"
      )
    }
    tryCatch(
      str2lang(call) |> eval(),
      error = function(x)
        output$plot_syntax <- renderText(paste0(res$error_msg$plot, "\n\n", x))
    )
  })


  observeEvent(input$scplot_examples, {
    selects <- input$scplot_examples
    id <- which(names(res$choices$scplot_examples) %in% selects)
    values <- paste0(unname(res$choices$scplot_examples[id]), collapse = "\n")
    if (length(id) == 0) values <- ""
    if ("(empty selection)" %in% selects) {
      values <- ""
      updateSelectInput(
        inputId = "scplot_examples", selected = ""
      )
    }
    updateTextAreaInput(inputId = "plot_arguments", value = values)
  })

  observeEvent(input$plot_arguments, render_plot_syntax())

  output$plot_scdf <- renderPlot({
    render_plot()
  })

  output$saveplot <- downloadHandler(
    filename = function() "my_scan_plot.png",
    content = function(file) {
      if (input$plot == "scplot"){
        ggplot2::ggsave(
          file, render_plot(), width = input$width, height = input$height,
          dpi = input$dpi, units = "px",  device = "png"
        )
      }
      if (input$plot == "plot.scdf"){
        grDevices::png(file, width = input$width, height = input$height,
                       res = input$dpi, units = "px")
        call <- paste0(
          "plot(transformed(), ", trim(input$plot_arguments), ")"
        )
        str2lang(call) |> eval()
        grDevices::dev.off()
      }
    }
  )

  render_plot_syntax <- reactive({
    if (input$plot == "scplot") {
      call <- paste0("scplot(scdf)")
      if (trimws(input$plot_arguments) != "") {
        call <- paste0(
          call, "%>%\n  ", gsub("\n", " %>%\n  ", trimws(input$plot_arguments))
        )
      }
    } else if (input$plot == "plot.scdf") {
      if (trim(input$plot_arguments) != "") {
        call <- paste0("plot(scdf, ", trim(input$plot_arguments), ")")
      } else {
        call <- "plot(scdf)"
      }
    }
    output$plot_syntax <- renderPrint({
      cat(call)
    })

  })

}
