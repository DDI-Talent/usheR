#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(DT)

# Load functions
source("../R/attendance_function.R")
source("../R/pairs-maker.R")

# Define file path to pairing history file
pair_history_path <- "student_pairs.csv"

# Define UI for application that generates student pairs
ui <- fluidPage(

    # Application title
    titlePanel("Student Pair Generator"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          fileInput("attendance_file", "Upload attendance CSV"),
          numericInput("group_size", "Group size", value = 2, min = 2),
          actionButton("generate", "Generate Pairs"),
          br(),
          downloadButton("download_pairs", "Download Pairs")
          ),

        mainPanel(
          DT::dataTableOutput("pair_table")
        )
    )
)

# Define server logic
server <- function(input, output, session) {

  generated_pairs <- reactiveVal()

  observeEvent(input$generate, {
    req(input$attendance_file)

    attendance <- read.csv(input$attendance_file$datapath)

    # Run pairing
    pair_table <- student_pairs(
      attendance = attendance,
      group_size = input$group_size,
      file_path = pair_history_path  # read/write pairing history
    )

    generated_pairs(pair_table)
  })

  output$pair_table <- DT::renderDataTable({
    df <- generated_pairs()
    req(df)

    # Make pairs more readable
    df$pairing <- gsub("~~", " & ", df$pairing)
    colnames(df) <- c("Week", "Group", "Student Pair")

    DT::datatable(
      df,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        order = list(list(0, 'desc')),
        columnDefs = list(list(className = 'dt-left', targets = "_all"))
      ),
      class = "compact stripe hover"
    )
  })

  output$download_pairs <- downloadHandler(
    filename = function() {
      paste0("student_pairs_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(generated_pairs(), file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
