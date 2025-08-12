
source('modules.R')

ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$head(
    tags$style(HTML('
    .group-container {
      border: 1px solid #ccc;
      padding: 10px;
      margin-bottom: 10px;
      background-color: #f9f9f9;
      width: 60%;
    }
    .group {
      font-weight: bold;
      font-size: 1.5em;
      color: #0000ff;
    }
    .individuals {
      font-size: 2rem;
    }'
    ))),

  titlePanel("Pair programming attendance and pairing"),

  tabsetPanel(id = 'tabs', selected = 'att_a',
    tabPanel('Instructions',

    ),
    tabPanel('Attendance (1)', value = 'att_a',
             selectAttendingStudentsUI('presence_tab')
    ),

    tabPanel('Attendance (2)', value = 'att_b',
             attendanceDTUI('attendance_alt')
    ),

    tabPanel('Pair',
             paringUI('pairing_tab')
    ),
    tabPanel('Attendance history',
             attendanceRecordUI('attendance_record_tab')
    )
  )
)


server <- function(input, output) {

  present_students <- reactiveVal()

  observe({
     if (input$tabs == 'att_a') present_students(selectAttendingStudents('presence_tab'))
     else if (input$tabs == 'att_b') present_students(attendanceDT('attendance_alt'))
  })



  paring('pairing_tab', present_students())
  attendanceRecord('attendance_record_tab',
                         current_tab = reactive(input$tabs))

}

# Run the application
shinyApp(ui = ui, server = server)


