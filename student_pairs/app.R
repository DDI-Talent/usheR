#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Load functions
source("R/attendance_function.R")
source("R/pairs-maker.R")

# Define UI for application that generates student pairs
ui <- fluidPage(

    # Application title
    titlePanel("Student Pair Generator"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          fileInput("attendance_file", "Upload attendance CSV"),
          numericInput("group_size", "Group size", value = 2, min = 2),
          numericInput("population", "Search iterations", value = 1000, min = 10),
          actionButton("generate", "Generate Pairs"),
          br(),
          downloadButton("download_pairs", "Download Pairs")
          ),

        mainPanel(
          tableOutput("pair_table")
        )
    )
)

# Define server logic
server <- function(input, output, session) {

  pairs_data <- reactiveVal()

  observeEvent(input$generate, {
    req(input$attendance_file)

    attendance <- read.csv(input$attendance_file$datapath)

    # Run pairing
    pairs <- student_pairs(
      attendance = attendance,
      group_size = input$group_size,
      population = input$population
    )

    pairs_data(pairs)
  })

  output$pair_table <- renderTable({
    pairs_data()
  })

  output$download_pairs <- downloadHandler(
    filename = function() {
      paste0("student_pairs_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(pairs_data(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
