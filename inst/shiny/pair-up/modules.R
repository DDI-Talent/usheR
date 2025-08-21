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


uploadClassDataUI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns('class_history_file'),
              'Choose Attendance File',
              accept =  '.csv'
    )
  )
}

uploadClassData <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      reactive({
        req(input$class_history_file)

        d <- read.csv(input$class_history_file$datapath)
        names(d) <- tolower(names(d))

        d <- if (hasName(d, 'week')) {
          setNames(split(d, d$week == 0), c('pairs_df', 'class_list'))
        } else {
          list(pairs_df = NULL, class_list = cbind('week' = 0, d))
        }

        d$class_list <- d$class_list[order(d$class_list$name), , drop = FALSE]
        d
      })

    })
}





selectAttendingStudentsUI <- function(id) {
  ns <- NS(id)
  tagList(

    sidebarLayout(
      sidebarPanel( width = 4,
                    uploadClassDataUI(ns('upload_class_data')),
                    uiOutput(ns('make_student_selection'))
      ),

      mainPanel(
        column(width = 3, style = 'margin-top:20px; font-size: 1.2em;',
               htmlOutput(ns('n_present'))
        ),
        column(width = 8,
               tableOutput(ns('present_students'))
        )
      )
    )
  )
}

selectAttendingStudents <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      class_data <- uploadClassData('upload_class_data')
      observe({
        click_count <- input$tgl_present_absent %% 2 + 1
        class_in_ex <- c('INcluded', 'EXcluded')
        btn_lab <- sprintf('Students selected below will be <span class=%s>%s</span>',
                           class_in_ex[click_count], class_in_ex[click_count])

        shinyjs::runjs(sprintf('
            var selectize_el = document.getElementsByClassName("selectize-input")[0];
              selectize_el.classList.remove("%s");
              selectize_el.classList.add("%s");
            ', class_in_ex[3 - click_count], class_in_ex[click_count]))


        updateActionButton(inputId = 'tgl_present_absent', label = btn_lab)
      }) |> bindEvent(input$tgl_present_absent)

      output$make_student_selection <- renderUI({
        # req(
        #   input$class_history_file,
        #   length(class_data() == 2),
        #   hasName(class_data()$class_list, 'name')
        # )

        tagList(
          actionButton(ns('tgl_present_absent'), HTML('Students selected below will be <span class=INcluded>INcluded</span>')
          ),
          # saveClassDataUI(ns('save_class_data')),
          selectizeInput(ns('select_students'), 'Choose students',
                         c('Everyone', class_data()$class_list$name), multiple = TRUE)
        )
      })

      available_for_pairing <- reactive({

        filter_by_selection <- if ('everyone' %in% tolower(input$select_students)) TRUE
        else expr(class_data()$class_list$name %in% input$select_students)

        if (isTruthy(input$tgl_present_absent %% 2)) filter_by_selection <- expr(!(!!filter_by_selection))

        class_data()$class_list |>
          select(name) |>
          filter(!!filter_by_selection)
      })

      output$n_present <- renderText({
        req(length(input$select_students) > 0)
        HTML(paste(p('You should have:'), div(style = 'width: 100%; text-align: center;', p(span(style='text-align: center; font-size: 1.2em; color: #2297E6;>', nrow(available_for_pairing()))), p('students in the class?'))))
      })

      output$present_students <- renderTable({
        data_is_valid <- tryCatch(hasName(class_data()$class_list, 'name') &&
                                    !grep('~~', class_data()$class_list), error = \(e) FALSE)
        # req(input$class_history_file)
        # validate(
        #   need(isTruthy(class_data()$class_list), 'Upload class list to select students'),
        #   need(length(class_data()) == 2, 'Upload class list to select studentsdi'),
        #   need(data_is_valid, 'Ensure class list has a `name` column')
        # )

        # class_data()$class_list
        available_for_pairing()
      })

      #       attendance_list <- reactive({
      #         ## todo: tidy this up a bit to make clearer
      #         ### esp. if available_for_pairing was a vector instead
      #         take_attendance(class_list(),
      #                         match(available_for_pairing()$name, class_list()$name),
      #                         session_id = sprintf('week_%02i', ncol(class_list()))
      #
      #         )
      #       })
      saveClassData('save_class_data',
                    reactive(dplyr::bind_rows(class_list())),
                    input$attendance_file, reactive(input$select_students))
      #
      #
      reactive(available_for_pairing()$name)

    })
}

pairingUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        numericInput(ns('group_size'), 'Group Size', 2, min = 2),
        actionButton(ns('btn_pair'), 'Pair Up'),
        # actionButton(ns('btn_copy_pairs'), HTML(paste(icon('clipboard'), 'Copy to clipboard', collapse = ''))),
        # saveClassDataUI(ns('save_pairs'), 'Save pairs')
      ),
      mainPanel(
        htmlOutput(ns('pairs'))
      )
    )
  )
}

pairing <- function(id, attendance) {
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

      # saveClassData('save_pairs', pairs_list, 'pairs.csv', reactive(input$btn_pair))
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








saveClassDataUI <- function(id, label = "Save to CSV") {
  ns <- NS(id)
  tagList(
    shinyjs::hidden(
      span(downloadButton(ns("download"), label, ),
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



saveClassData <- function(id, d,
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