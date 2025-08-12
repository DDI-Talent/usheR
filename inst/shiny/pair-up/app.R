source('modules.R')

ui <- fluidPage(
  shinyjs::useShinyjs(),
  includeCSS('www/style.css'),
  tabsetPanel(id = 'tabs', selected = 'take_att',
              tabPanel('Instructions',
                       instructions('instr')
              ),
              tabPanel('Take Attendance', value = 'take_att',
                       selectAttendingStudentsUI('presence_tab')
              ),
              tabPanel('Pair',
                       paringUI('pairing_tab')
              ),
              tabPanel('Review Attendance',
                       reviewAttendanceUI('attendance_record_tab')
              )
  )
)


server <- function(input, output) {
  present_students <- selectAttendingStudents('presence_tab')
  paring('pairing_tab', present_students)
  reviewAttendance('attendance_record_tab',
                   current_tab = reactive(input$tabs))
}

# Run the application
shinyApp(ui = ui, server = server)




