create_pair_divs <- function(pairs, gr_num) {
  pairs <- strsplit(pairs, '~~')[[1]]

  g <- htmltools::p(class = 'pair-group', paste('Group', gr_num))
  ul <- htmltools::tags$ul(class = 'individuals', lapply(pairs, tags$li))

  paste(
    htmltools::tags$div(class = 'pair-group-container', g, '\n', ul)
  ) |>
    htmltools::HTML()
}

prep_for_copy <- function(d) {
  d[d$week != 0, 'name'] |>
    strsplit('~~') |>
    unname() |>
    purrr::imap_chr(
      ~ sprintf('Group %02i\n%s\n', .y, paste0('  - ', .x, collapse = '\n')
      )
    )
}

uploadClassDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns('class_history_file'), 'Choose Attendance File',
              accept = '.csv'
    )
  )
}

uploadClassData <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    reactive({
      req(input$class_history_file)

      d <- read.csv(input$class_history_file$datapath)
      names(d) <- tolower(trimws(names(d)))

      d <- d[names(d) %in% c('week', 'name')]

      d <- if (!hasName(d, 'week')) {
        list(pairs_df = NULL, class_list = cbind('week' = 0, d))
      } else {
        list(pairs_df = d[d$week != 0, ], class_list = d[d$week == 0, ])
      }

      if (hasName(d, 'name')) {
        # stop('[Unhandled error] The selected file has no `name` column')
        # d <- data.frame(week = 0)
        d$class_list <- d$class_list[order(d$class_list$name), , drop = FALSE]
      }

      list(
        class_list = d$class_list,
        pair_history = d$pairs_df,
        file_meta = input$class_history_file
      )
    })
  })
}


selectAttendingStudentsUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 4,
        uploadClassDataUI(ns('upload_class_data')),
        uiOutput(ns('make_student_selection'))
      ),

      mainPanel(
        column(
          width = 3,
          style = 'margin-top:20px; font-size: 1.2em;',
          htmlOutput(ns('n_present'))
        ),
        column(width = 8, tableOutput(ns('present_students')))
      )
    )
  )
}

selectAttendingStudents <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    class_data_and_metadata <- uploadClassData('upload_class_data')
    class_list   <- reactive({ class_data_and_metadata()$class_list })
    pair_history <- reactive({ class_data_and_metadata()$pair_history })
    file_info    <- reactive({ class_data_and_metadata()$file_meta })

    observe({ tgl_select_btn_visibility(input, 'tgl_present_absent') }) |>
      bindEvent(input$tgl_present_absent)

    output$make_student_selection <- renderUI({
      validate_class_data(file_info, class_list)

      output$n_present <- renderText({
        HTML(paste(
          p('You should have:'),
          div(
            style = 'width: 100%; text-align: center;',
            p(span(
              style = 'text-align: center; font-size: 1.2em; color: #2297E6;>',
              nrow(available_for_pairing())
            )),
            p('students in the class?')
          )
        ))
      })

      tagList(
        actionButton(
          ns('tgl_present_absent'),
          HTML(
            'Students selected below will be <span class=INcluded>INcluded</span>'
          )
        ),
        # saveClassDataUI(ns('save_class_data')),
        selectizeInput( ns('select_students'), 'Choose students',
          c('Everyone', class_list()$name),
          selected = 'Everyone', multiple = TRUE
        )
      )
    })

    available_for_pairing <- reactive({
      validate_class_data(file_info, class_list)
      filter_student_selection(input, class_list)
    })

    output$present_students <- renderTable({
      data_is_valid <- forceReactiveEval({
        stopifnot('Select a `class data` file.' = try(isTruthy(file_info())))
        stopifnot(
          'Data must have `name` column.' = try(hasName(class_list(), 'name'))
        )
        stopifnot(
          'Data must have a `week 0` containing individual student names.\nSee `Instructions tab` for required format.' = try( nrow(class_list()) > 0)
        )
        stopifnot(
          'All weeks other that 0 must contain only paired names.' = try(all(grepl( '~~', pair_history()$name)))
        )
        stopifnot(
          'Your data contain only pre-paired names.' = try( !any(grepl('~~', class_list()$name))
          )
        )
      })

      validate( need(data_is_valid$truthy, data_is_valid$message) )

      available_for_pairing()
    })

    combined_data <- reactive(
      dplyr::bind_rows(
        class_list(),
        pair_history()
      )
    )

    saveClassData(
      'save_class_data',
      combined_data,
      file_info,
      reactive(input$select_students)
    )

    reactive(
      list(
        available = available_for_pairing(),
        class_data = class_data_and_metadata()
      )
    )
  })
}

pairPresentStudentsUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        numericInput(ns('group_size'), 'Group Size', 2, min = 2),
        actionButton(ns('btn_pair'), 'Pair Up'),
        # actionButton(
        #   ns('btn_copy_pairs'),
        #   HTML(paste(icon('clipboard'), 'Copy to clipboard', collapse = ''))
        # ),
        saveClassDataUI(ns('save_pairs'), 'Save pairs')
      ),
      mainPanel(
        htmlOutput(ns('new_pairs'))
      )
    )
  )
}

pairPresentStudents <- function(id, attendance) {
  moduleServer(id, function(input, output, session) {
    observe({ tgl_pair_btn_visibility(attendance) })

    shinyjs::disable('btn_copy_pairs')
    observe({
      req(input$btn_pair, pairs_list())
      shinyjs::enable('btn_copy_pairs')
    })

    observe({
      req(F)
      clipr::write_clip(pairs_list(), allow_non_interactive = TRUE)
    }) |>
      bindEvent(input$btn_copy_pairs)

    pairs_list <- reactive({
      l <- attendance()$class_data$pair_history
      if (!is.null(l)) l <- convert_to_list(l)

      create_pairs(
        attendance()$available$name,
        group_size = input$group_size,
        record = l
      )
    }) |>
      bindEvent(input$btn_pair)

    output$new_pairs <- renderText({
      purrr::imap_chr(pairs_list()[[1]], create_pair_divs) |>
        paste0(collapse = '') |>
        htmltools::HTML()
    })

    observe( input$btn_pair ) |>
      bindEvent(input$btn_pair)

    combined_data <- reactive({
      bind_rows(
        attendance()$class_data$class_list,
        convert_to_df(pairs_list()) |> mutate(week = as.integer(week)),
      )
    })

    file_info <- reactive({ attendance()$class_data$file_meta })

    observe({
      saveClassData(
        'save_pairs',
        combined_data,
        file_info,
        reactive(input$btn_pair)
      )
    })
  })
}

reviewAttendanceUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        actionButton(ns('togl_attendance'), 'Show / Hide Attendance'),
      ),
      mainPanel(
        tableOutput(ns('attendance_history'))
      )
    )
  )
}

reviewAttendance <- function(id, current_tab) {
  moduleServer(id, function(input, output, session) {
    # current_tab = reactive(input$tabs)
    output$attendance_history <- renderTable(
      {
        read.csv('large_class_list.csv') |>
          dplyr::mutate(
            attendance = as.integer(rowSums(across(where(is.logical)))),
            dplyr::across(where(is.logical), ~ ifelse(., 'X', ''))
          )
      },
      striped = TRUE,
      align = 'r'
    )

    shinyjs::hide('attendance_history')
    observe({
      shinyjs::toggle('attendance_history')
    }) |>
      bindEvent(input$togl_attendance)
    observe({
      shinyjs::hide('attendance_history')
    }) |>
      bindEvent(current_tab())
  })
}


saveClassDataUI <- function(id, label = "Save to CSV") {
  ns <- NS(id)
  tagList(
    shinyjs::hidden(
      span(
        downloadButton(ns("download"), label, ),
        id = ns('save-btn-container'),
        container = 'inline'
      )
    )
  )
}


save_output <- function(dataframe, file_path) {
  tryCatch(
    {
      existing <- readr::read_csv(file_path, show_col_types = FALSE)
      dataframe <- dplyr::full_join(dataframe, existing, by = "name")
    },
    error = \(e) dataframe
  )

  write.csv(dataframe, file_path, row.names = FALSE)

  dataframe
}


saveClassData <- function(id, d, write_to, show_save_button) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      if (isTruthy(show_save_button())) {
        shinyjs::show("save-btn-container")
      } else {
        shinyjs::hide("save-btn-container")
      }
    })

    output$download <- downloadHandler(
      filename = function() {
        write_to()$name
      },
      content = function(file) {
        write.csv(d(), file, row.names = FALSE)
      }
    )
  })
}
