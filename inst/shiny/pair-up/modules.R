library(usheR)
library(dplyr)
library(shiny)
library(shinyjs)
library(clipr)

create_pair_divs <- function(pairs, gr_num) {
  pairs <- strsplit(pairs, '~~')[[1]]

  g <- htmltools::p(class = 'pair-group', paste('Group', gr_num))
  ul <- htmltools::tags$ul(class = 'individuals', lapply(pairs, tags$li))

  paste(
    htmltools::tags$div(class = 'pair-group-container', g, '\n', ul
    )) |>
    htmltools::HTML()
}

instructions <- function(id) {
  ns <- NS(id)
  NULL
  # includeMarkdown('www/instructions.Rmd')
  # includeHTML('www/instructions.html')
  # source('www/_tmp_ui-q-instr.R')$value
}

selectAttendingStudentsUI <- function(id) {
  ns <- NS(id)
  tagList(

    sidebarLayout(
      sidebarPanel(width = 4,
        fileInput(ns('attendance_file'),
                  'Choose Attendance File',
                  accept =  '.csv'
        ),
        uiOutput(ns('make_student_selection'))
      ),

      mainPanel(
        textOutput(ns('n_present')),
        tableOutput(ns('present_students'))
      )
    )
  )
}

selectAttendingStudents <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      class_list <- reactive({
        d <- read.csv('large_class_list.csv')
        d <- read.csv(input$attendance_file$datapath)
        names(d) <- tolower(names(d))
        d[order(d[[1]]), , drop = FALSE]
      })


      observe({
        updateActionButton(inputId = 'tgl_present_absent',
                           label = c('Add students', 'Remove students')[input$tgl_present_absent %% 2 + 1])
      }) |> bindEvent(input$tgl_present_absent)

      output$make_student_selection <- renderUI({
        req(input$attendance_file,
            hasName(class_list(), 'name'))
        tagList(
          actionButton(ns('tgl_present_absent'), 'Add students'),
          saveUI(ns('save_attendance'), 'Save attendance'),
          selectizeInput(ns('select_students'), 'Present',
                         c('Everyone', class_list()$name), multiple = TRUE)
        )
      })



      available_for_pairing <- reactive({


        filter_by_selection <- if ('Everyone' %in% input$select_students) TRUE
                               else expr(class_list()$name %in% input$select_students)
        if (isTruthy(input$tgl_present_absent %% 2)) filter_by_selection <- expr(!(!!filter_by_selection))

        class_list() |>
          select(name) |>
          filter(!!filter_by_selection)
      }) # output would probably be better as a vector

      output$n_present <- renderText({
        req(length(input$select_students) > 0)
        paste('Class size: ', nrow(available_for_pairing()))
      })

      output$present_students <- renderTable({
        data_is_valid <- tryCatch(hasName(class_list(), 'name'), error = \(e) F)
        validate(
          need(input$attendance_file, 'Upload class list to select students'),
          need(data_is_valid, 'Ensure class list has a `name` column')
        )

        available_for_pairing()
      })

      attendance_list <- reactive({
        ## TODO: tidy this up a bit to make clearer
        ### esp. if available_for_pairing was a vector instead
        take_attendance(class_list(),
                        match(available_for_pairing()$name, class_list()$name),
                        session_id = sprintf('week_%02i', ncol(class_list()))

        )
      })
      saveServer('save_attendance',
                 reactive(dplyr::left_join(class_list(), attendance_list(), by = "name")
),
                 input$attendance_file, reactive(input$select_students))


      reactive(available_for_pairing()$name)

    })
}

paringUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        numericInput(ns('group_size'), 'Group Size', 2, min = 2),
        actionButton(ns('btn_pair'), 'Pair Up'),
        actionButton(ns('btn_copy_pairs'), HTML(paste(icon('clipboard'), 'Copy to clipboard', collapse = ''))),
        saveUI(ns('save_pairs'), 'Save pairs')
      ),
      mainPanel(
          htmlOutput(ns('pairs'))
      )
    )
  )
}

paring <- function(id, attendance) {
  moduleServer(
    id,
    function(input, output, session) {
      shinyjs::disable('btn_copy_pairs')

      observe({
        req(input$btn_pair, pairs_list())
        shinyjs::enable('btn_copy_pairs')
      })

      observe({
        req(F)
        clipr::write_clip(pairs_list(), allow_non_interactive = TRUE)
      }) |> bindEvent(input$btn_copy_pairs)


      pairs_list <- reactive({
        create_pairs(attendance(), group_size = input$group_size)[[1]]
        # create_pairs(attendance()$name, group_size = input$group_size)[[1]]
      }) |>
        bindEvent(input$btn_pair)

      output$pairs <- renderText({
        purrr::imap_chr(pairs_list(), ~create_pair_divs(.x, .y)) |>
          paste0(collapse = '') |> htmltools::HTML()
      })

      # saveServer('save_pairs', pairs_list, 'pairs.csv', reactive(input$btn_pair))
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
  moduleServer(
    id,
    function(input, output, session) {
      # current_tab = reactive(input$tabs)
      output$attendance_history <- renderTable({

        read.csv('large_class_list.csv') |>
          dplyr::mutate(attendance = as.integer(rowSums(across(where(is.logical)))),
                        dplyr::across(where(is.logical), ~ ifelse(., 'X', '')))
      }, striped = TRUE, align = 'r')

      shinyjs::hide('attendance_history')
      observe({
        shinyjs::toggle('attendance_history')

      }) |> bindEvent(input$togl_attendance)
      observe({
        shinyjs::hide('attendance_history')

      }) |> bindEvent(current_tab())
    })
}








saveUI <- function(id, label = "Download CSV") {
  ns <- NS(id)
  tagList(
    shinyjs::hidden(
      span(downloadButton(ns("download"), label),
          id = ns('save-btn-container'), container = 'inline')
    )
  )
}




save_output <- function(dataframe,
                        file_path) {

  # if (!is.null(file_path)) {
  #   if (file.exists(file_path)) {
  #     existing <- readr::read_csv(file_path, show_col_types = FALSE)
  #     dataframe <- dplyr::full_join(dataframe, existing, by = "name")
  #   }

    tryCatch({
      existing <- readr::read_csv(file_path, show_col_types = FALSE)
      dataframe <- dplyr::full_join(dataframe, existing, by = "name")
    },
    error = \(e) dataframe
    )


    write.csv(dataframe, file_path, row.names = FALSE)

  dataframe
  }



saveServer <- function(id, d,#r1, dr2,
                       write_to, show_save_button) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    observe({
      if (isTruthy(show_save_button())) shinyjs::show("save-btn-container")
      else shinyjs::hide("save-btn-container")
    })



    output$download <- downloadHandler(
      filename = function() {
        file_input <- reactive(write_to)
        file_input()$name
      },
      content = function(file) {
        write.csv(d(), file, row.names = FALSE)
      }
    )
  })
}
