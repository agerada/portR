# UI for standard app
library(shiny)

fluidPage(
  titlePanel("Standard App with Models"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose Dataset:",
                  choices = c("mtcars", "iris")),
      actionButton("predict", "Run Prediction")
    ),
    
    mainPanel(
      tableOutput("results"),
      plotOutput("plot")
    )
  )
)
