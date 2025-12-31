# Server for standard app
library(shiny)

function(input, output, session) {
  
  # Load model from models directory
  model <- reactive({
    # In real app, would load from models/model.rds
    lm(mpg ~ wt, data = mtcars)
  })
  
  data <- reactive({
    if (input$dataset == "mtcars") mtcars else iris
  })
  
  output$results <- renderTable({
    head(data())
  })
  
  output$plot <- renderPlot({
    plot(data()[, 1:2])
  })
}
