# Entry script that sources other files
# This tests the edge case where the entry script depends on sourced files

# Source utility functions
source("R/utils.R")
source("R/data_processing.R")
source("R/plot_helpers.R")

# Load configuration
config <- load_config("config/settings.yaml")

# Run the app
library(shiny)

ui <- fluidPage(
  titlePanel(config$app_title),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Data"),
      actionButton("process", "Process")
    ),
    mainPanel(
      plotOutput("main_plot"),
      verbatimTextOutput("summary")
    )
  )
)

server <- function(input, output, session) {
  
  processed_data <- eventReactive(input$process, {
    req(input$file)
    df <- read.csv(input$file$datapath)
    process_data(df)  # From R/data_processing.R
  })
  
  output$main_plot <- renderPlot({
    req(processed_data())
    create_summary_plot(processed_data())  # From R/plot_helpers.R
  })
  
  output$summary <- renderPrint({
    req(processed_data())
    format_summary(processed_data())  # From R/utils.R
  })
}

shinyApp(ui, server)
