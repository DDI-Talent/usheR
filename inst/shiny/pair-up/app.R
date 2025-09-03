# library(usheR)
library(dplyr)
library(shiny)
library(shinyjs)
source('helpers.R')
source('modules.R')
# needed until merged
source('_tmp-helpers.R')
version <- '0.1.0'

ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel(
    HTML(paste0(
      'Pair programmng pairing app ',
      span(paste0('v', version), style = 'color: #E67E22')
    ))
  ),
  # includeCSS('www/style.css'),
  tags$head(tags$link(
    rel = "stylesheet",
    type = "text/css",
    href = "style.css"
  )),
  tabsetPanel(
    id = 'tabs',
    selected = 'take_att',
    type = 'pills',
    tabPanel('ToDo', includeHTML('www/dev-notes.html')),
    tabPanel('Instructions', includeHTML('www/instructions.html')),
    tabPanel(
      'Take Attendance',
      value = 'take_att',
      selectAttendingStudentsUI('presence_tab')
    ),
    tabPanel('Pair', pairPresentStudentsUI('pairing_tab')),
    # tabPanel('Review Attendance',
    #          reviewAttendanceUI('attendance_record_tab')
  )
)


server <- function(input, output) {
  present_students <- selectAttendingStudents('presence_tab')
  pairPresentStudents('pairing_tab', present_students)
  # reviewAttendance('attendance_record_tab',
  #                  current_tab = reactive(input$tabs))
}

# Run the application
shinyApp(ui = ui, server = server, options = list(port = 7909))
