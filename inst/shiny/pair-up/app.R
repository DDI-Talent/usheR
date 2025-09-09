# library(usheR)
library(dplyr)
library(shiny)
library(shinyjs)
source('global.R')
source('_tmp-helpers.R')
source('modules.R')

tab_title <- 'Pair programmng pairing app '
app_version <- '0.4.0'
app_title <- HTML(paste0(
  tab_title,
  span(paste0('v', app_version), style = 'font-size: .5em; color: #9B59B6')
))

ui <- fluidPage(
  shinyjs::useShinyjs(),
  title = tab_title,
  titlePanel(app_title),
  tags$head(tags$link( rel = "stylesheet", type = "text/css", href = "style.css")),

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
    tabPanel('Pair', pairPresentStudentsUI('pairing_tab'))
)
)


server <- function(input, output) {
  present_students <- selectAttendingStudents('presence_tab')
  pairPresentStudents('pairing_tab', present_students)
}

# Run the application
shinyApp(ui = ui, server = server, options = list(port = 7909))
