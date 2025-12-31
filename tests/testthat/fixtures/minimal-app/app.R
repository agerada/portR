# Minimal Shiny App
# Single file, no dependencies beyond shiny

library(shiny)

ui <- fluidPage(

  titlePanel("Minimal App"),
  mainPanel(
    textOutput("message")
  )
)

server <- function(input, output, session) {
  output$message <- renderText("Hello, World!")
}

shinyApp(ui, server)
