# Setup script that defines the application runner

run_application <- function(port = 3838) {
  library(shiny)
  
  ui <- fluidPage(
    titlePanel("Nested Entry Point App"),
    mainPanel(
      textOutput("info")
    )
  )
  
  server <- function(input, output, session) {
    output$info <- renderText({
      paste("Running on port:", port)
    })
  }
  
  shinyApp(ui, server, options = list(port = port))
}
