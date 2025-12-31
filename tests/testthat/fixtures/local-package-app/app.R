# App with local package dependency
# Tests the edge case where renv.lock contains a local package

library(shiny)
library(mylocalpackage)  # This would be a local package

ui <- fluidPage(
  titlePanel("App with Local Package"),
  mainPanel(
    textOutput("result")
  )
)

server <- function(input, output, session) {
  output$result <- renderText({
    # mylocalpackage::my_function()
    "Using local package functionality"
  })
}

shinyApp(ui, server)
