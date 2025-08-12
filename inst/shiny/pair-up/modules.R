library(usheR)
library(shiny)
library(dplyr)
library(shinyjs)
library(DT)
library(clipr)

format_pair_list <- function(pairs, gr_num) {
  pairs <- strsplit(pairs, '~~')[[1]]

  g <- htmltools::p(paste('Group', gr_num), class = 'group')
  ul <- htmltools::tags$ul(class = 'individuals', lapply(pairs, tags$li))

  paste(
    htmltools::tags$div(class = 'group-container', g, '\n', ul
    )
  ) |>
    htmltools::HTML()
}

instructions <- function(id) {
  ns <- NS(id)
  includeHTML('www/instructions.html')
}

selectAttendingStudentsUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
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
        d <- read.csv(input$attendance_file$datapath)
        d[order(d[[1]]), , drop = FALSE]
      })


      observe({
        updateActionButton(inputId = 'tgl_present_absent',
                           label = c('Present', 'Absent')[input$tgl_present_absent %% 2 + 1])
      }) |> bindEvent(input$tgl_present_absent)

      output$make_student_selection <- renderUI({

        tagList(
          actionButton(ns('tgl_present_absent'), 'Present'),

          selectizeInput(ns('select_students'), 'Present',
                         class_list()$name, multiple = TRUE)
        )
      })



      available_for_pairing <- reactive({
        req(class_list(), input$select_students)
        validate(
          need(hasName(class_list(), 'name'),
               'Class list data must have a name column'
          )
        )

        filter_by_selection <- expr(class_list()$name %in% input$select_students)
        if (isTruthy(input$tgl_present_absent %% 2)) filter_by_selection <- expr(!(!!filter_by_selection))

        class_list() |>
          select(name) |>
          filter(!!filter_by_selection)
      })

      output$n_present <- renderText({
        req(input$select_students)
        paste('Class size: ', nrow(available_for_pairing()))
      })

      output$present_students <- renderTable({ available_for_pairing() })

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
        dlCsvUI(ns('save'), 'Save pairs')
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

      }) |>
        bindEvent(input$btn_pair)

      output$pairs <- renderText({
        purrr::imap_chr(pairs_list(), ~format_pair_list(.x, .y)) |>
          paste0(collapse = '') |> htmltools::HTML()
      })


      dlCsv('save', pairs_list, 'pairs.csv')
    })
}

attendanceRecordUI <- function(id) {
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

attendanceRecord <- function(id, current_tab) {
  moduleServer(
    id,
    function(input, output, session) {

      output$attendance_history <- renderTable({

        read.csv('large_class_list.csv') |>
          dplyr::mutate(attendance = as.integer(rowSums(across(where(is.logical)))),
                        dplyr::across(where(is.logical), ~ ifelse(., 'X', '')))
      }, striped = TRUE, align = 'r', )

      shinyjs::hide('attendance_history')
      observe({
        shinyjs::toggle('attendance_history')

      }) |> bindEvent(input$togl_attendance)
      observe({
        shinyjs::hide('attendance_history')

      }) |> bindEvent(current_tab())
    })
}

attendanceDTUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(ns('attendance_file'),
                  'Choose Attendance File',
                  accept =  '.csv'
        ),

        invertSelectionUI(ns('invert')),
        dlCsvUI(ns('save'), 'Save attendance')
      ),

      mainPanel(
        textOutput(ns('n_present')),
        dataTableOutput(ns('dt_table'))
      )
    )
  )
}

attendanceDT <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      class_list <- reactive({
        d <- read.csv(input$attendance_file$datapath)
        d[order(d[[1]]), , drop = FALSE]
      })

      output$dt_table <- DT::renderDT({
        DT::datatable(
          class_list(),
          options = list(
            dom = "t",
            pageLength = nrow(class_list()),
            ordering = FALSE
          ),
          rownames = FALSE,
          width = '100%'
        )
      })

      selected_students <- reactive({
        idx <- as.integer(input$dt_table_rows_selected)

        class_list()[idx, 'name']
      })

      output$n_present <- renderText({
        req(selected_students())
        paste('Present students (', length(selected_students()), ')')
      })

      invertSelection('invert', 'dt_table', class_list, reactive(input$dt_table_rows_selected))

    })
}



dlCsvUI <- function(id, label = "Download CSV") {
  ns <- NS(id)
  downloadButton(ns("download"), label)
}

dlCsv <- function(id, data_reactive, filename = "data.csv") {
  moduleServer(id, function(input, output, session) {
    output$download <- downloadHandler(
      filename = function() {
        filename
      },
      content = function(file) {
        write.csv(data_reactive(), file, row.names = FALSE)
      }
    )
  })
}


invertSelectionUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns('invert'), 'Invert Selection')
  )
}

invertSelection <- function(id, tableId, data, rows_selected) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    print("Creating proxy")
    proxy <- DT::dataTableProxy(tableId, session)

session$onFlushed(function() {
    observe({
      all_rows <- seq_len(nrow(data()))
      new_selection <- setdiff(all_rows, rows_selected())
      DT::selectRows(proxy, new_selection)
    }) |> bindEvent(input$invert)
  })
})
}
