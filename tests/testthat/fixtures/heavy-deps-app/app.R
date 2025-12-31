# Heavy dependencies app
# Tests with many CRAN packages

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(DT)
library(shinydashboard)
library(shinyWidgets)

ui <- dashboardPage(
  dashboardHeader(title = "Heavy Dependencies App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard"),
      menuItem("Data", tabName = "data"),
      menuItem("Analysis", tabName = "analysis")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
        fluidRow(
          valueBoxOutput("box1"),
          valueBoxOutput("box2"),
          valueBoxOutput("box3")
        ),
        fluidRow(
          box(plotlyOutput("main_plot"), width = 12)
        )
      ),
      tabItem(tabName = "data",
        DTOutput("data_table")
      ),
      tabItem(tabName = "analysis",
        plotOutput("analysis_plot")
      )
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    mtcars %>%
      mutate(car = rownames(mtcars)) %>%
      tidyr::pivot_longer(cols = c(mpg, hp, wt), names_to = "metric")
  })
  
  output$box1 <- renderValueBox({
    valueBox(nrow(mtcars), "Cars", icon = icon("car"))
  })
  
  output$box2 <- renderValueBox({
    valueBox(round(mean(mtcars$mpg), 1), "Avg MPG", icon = icon("gas-pump"))
  })
  
  output$box3 <- renderValueBox({
    valueBox(round(mean(mtcars$hp), 0), "Avg HP", icon = icon("bolt"))
  })
  
  output$main_plot <- renderPlotly({
    p <- ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
      geom_point(size = 3) +
      theme_minimal() +
      labs(title = "Weight vs MPG", color = "Cylinders")
    ggplotly(p)
  })
  
  output$data_table <- renderDT({
    datatable(mtcars, options = list(pageLength = 10))
  })
  
  output$analysis_plot <- renderPlot({
    ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
      geom_boxplot(fill = "steelblue") +
      theme_minimal() +
      labs(title = "MPG by Cylinder Count")
  })
}

shinyApp(ui, server)
