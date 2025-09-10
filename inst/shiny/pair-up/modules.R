uploadClassDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns('btn_reset'), 'Reset file', class = 'btn-danger',
               style = 'padding: 3px 4px; font-size: 0.8em;'),
    fileInput(
      ns('class_history_file'), 'Choose Attendance File', accept = '.csv')
  )
}

uploadClassData <- function(id) {
  moduleServer(id, function(input, output, session) {
    observe({ shinyjs::runjs('history.go(0)') }) |> 
      bindEvent(input$btn_reset)

    reactive({
      req(input$class_history_file)

      d <- read.csv(input$class_history_file$datapath) |> 
        validate_class_data() |> 
        split_data_concerns()

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
        width = 3,
        uploadClassDataUI(ns('upload_class_data')),
        uiOutput(ns('make_student_selection'))
      ),

      mainPanel(
        column(width = 3, style = 'margin-top:20px; font-size: 1.2em;',
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

    pp_session <- setPairProgSession('set_session', pair_history)

    output$make_student_selection <- renderUI({

      validateClassData(file_info, class_list)


      output$n_present <- renderText({
        HTML(
          paste(
            div(
              style = 'width: 100%; text-align: left; margin-bottom: 40px;',
              p('You should have:'),
              p(span(
                style = 'margin-left: 50px; font-size: 1.2em; color: #2297E6;>',
                nrow(available_for_pairing())
              )),
              p('students in the class?')
            ),
            div(
              style = 'background-color: #9B59B688; padding: 2px 10px 0 10px; font-size: 1.2em; text-align: left;',
              p('Remember to update the session name as appropriate!')
            )
        )
      )
    })

      tagList(
        div(
          style = 'margin-bottom: 30px;', # !! Doesn't actually work for some reason
          setPairProgSessionUI(ns('set_session')),
        ),
        
        actionButton(width = '100%',
          ns('tgl_present_absent'),
          HTML(
            'Students selected below will be <span class=INcluded>INcluded</span>'
          )
        ),
        p('(click to toggle)', style = 'text-align: right;'),
        div(class = 'select-students-input',
          selectizeInput( ns('select_students'), 'Choose students',
            choices = c('Everyone', class_list()$name),
            selected = 'Everyone', multiple = TRUE
          ) 
        )
      )
    })

    available_for_pairing <- reactive({
      validateClassData(file_info, class_list)
      filter_student_selection(input, class_list)
    })

    output$present_students <- renderTable({
      data_is_valid <- forceReactiveEval({
        stopifnot('Select a `class data` file, a file with a list of student names.' = try(isTruthy(file_info())))
        stopifnot('Your class list must have a `name` column containing your students\' names.\n[See error 1 in instructions]' = try(hasName(class_list(), 'name')))
        stopifnot('Missing `week 0` information with individual student names.\n[See error 2 in instructions]' = try(nrow(class_list()) > 0) )
        stopifnot('Weeks 1+ must contain only paired names - no individual names.\n[See error 3 in instructions]' = try(all(grepl('~~', pair_history()$name))))
        stopifnot('Week 0 cannot contain paired names. Use individual names only.\n[See error 4 in instructions]' = try(!any(grepl('~~', class_list()$name))))
      })

      validate(need(data_is_valid$truthy, data_is_valid$message))

      available_for_pairing()
    })


    reactive(
      list(
        available = available_for_pairing(),
        class_data = class_data_and_metadata(),
        pp_session = pp_session()
      )
    )
  })
}

pairPresentStudentsUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        width = 3,
        div(
          class = 'pair-controls',
          actionButton(ns('btn_pair'), 'Pair Up',
            class = 'pair-button', container = 'inline'
          ),
          numericInput(ns('group_size'), 'Group Size',
            value = 2, min = 2, width = '50%'
          )
        ),
        copyToClipboardUI(ns('btn_copy_pairs')),
        saveClassDataUI(ns('save_pairs'), 'Save pairs')
      ),
      mainPanel(
        width = 8,
        # htmlOutputns('new_pairs')
        uiOutput(ns('new_pairs'))
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

    # output$new_pairs <- renderText({
    #   p <- purrr::imap_chr(pairs_list()[[1]], create_pair_divs) |>
    #     paste0(collapse = '') |>
    #     htmltools::HTML()
    # })

    output$new_pairs <- renderUI({
      fluidRow(
        column(12,
          purrr::imap(pairs_list()[[1]], create_pair_divs)
        )
      )
    })

    observe(input$btn_pair) |>
      bindEvent(input$btn_pair)

    combined_data <- reactive({
      # This should make use of NBs convert_to_df() function
      most_recent_pairs <- convert_to_df(pairs_list()[[1]])
      most_recent_pairs$week <- as.integer(attendance()$pp_session$week)
      most_recent_pairs$session <- attendance()$pp_session$session

      bind_rows(
        attendance()$class_data$class_list,
        attendance()$class_data$pair_history,
        most_recent_pairs
      )
    })

    file_info <- reactive({ attendance()$class_data$file_meta })

    copyToClipboard('btn_copy_pairs', pairs_list, reactive(input$btn_pair))

    saveClassData(
      'save_pairs', combined_data,
      file_info, reactive(input$btn_pair)
    )
  })
}

reviewAttendanceUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        actionButton(ns('tgl_attendance'), 'Show / Hide Attendance'),
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
    output$attendance_history <- renderTable({
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
    observe({ shinyjs::toggle('attendance_history') }) |>
      bindEvent(input$tgl_attendance)

    observe({ shinyjs::hide('attendance_history') }) |>
      bindEvent(current_tab())
  })
}


saveClassDataUI <- function(id, label = 'Save to CSV') {
  ns <- NS(id)
  tagList(
    shinyjs::hidden(
      span(
        id = ns('save-btn-container'),
        downloadButton(ns('download'), label)
      )
    )
  )
}


saveClassData <- function(id, d, write_to, reveal_button) {
  moduleServer(id, function(input, output, session) {
    observe({
      if (isTruthy(reveal_button())) {
        shinyjs::show('save-btn-container')
      } else {
        shinyjs::hide('save-btn-container')
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


copyToClipboardUI <- function(id) {
  ns <- NS(id)
  span(
    id = ns('clipboard_container'),
    #actionButton(ns('copy_btn'), 'Copy to Clipboard', class = 'btn-info')
    uiOutput(ns('clip'), inline = TRUE)
  )
}

copyToClipboard <- function(id, data_reactive, reveal_button) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observe({
      if (reveal_button()) {
        shinyjs::show('clipboard_container')
      } else {
        shinyjs::hide('clipboard_container')
      }
    })

    observe({
        showNotification(
          'Groups copied to clipboard!',
          type = 'message',
          duration = 2
       )
     }) |> bindEvent(input$copy_btn)

    output$clip <- renderUI({
      data_reactive()[[1]] |>
        prep_for_copy() |>
        rclipboard::rclipButton(
          inputId = ns('copy_btn'),
          label = 'Copy to clipboard',
          clipText = _,
          icon = icon('clipboard'),
          class = 'btn btn-info'
        )
    })
  })
}


setPairProgSessionUI <- function(id) {
  ns <- NS(id)
  tagList(
    column(6,
      numericInput(ns('week_number'), 'Week Number', value = 1, min = 1),
    ),
    column(6,
      selectizeInput(ns('session_name'), 'Session Name',
        choices = NULL, options = list( create = TRUE)
      )
    )
  )
}

setPairProgSession <- function(id, pair_data = NULL) {
  moduleServer(id, function(input, output, session) {
    observe({
      req(pair_data)
      
      existing_sessions <- unique(pair_data()$session)
      existing_sessions <- existing_sessions[!is.na(existing_sessions)]
      week <- max(pair_data()$week, 1, na.rm = TRUE)
      
      updateNumericInput(
        session,
        'week_number',
        value = week
      )

      updateSelectizeInput(
        session,
        'session_name',
        choices = existing_sessions %||% 'Session 1',
        server = TRUE
      )
    })
    
    # Return the selected values
    reactive({
      list(
        week = input$week_number,
        session = input$session_name
      )
    })
  })
}
