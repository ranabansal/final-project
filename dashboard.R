# This is the dashboard for the application
#

library(shiny)
library(tidyverse)
library(shinydashboard)

# Define UI for the application
ui <- dashboardPage(
  dashboardHeader(title = "Indian Exports"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "dashboard", icon = icon("home")),
      menuItem("Top Export Partners", tabName = "exports", icon = icon("ship")),
      menuItem("Graphs", tabName = "graphs", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              h3("On Nov. 8, 2016, the Indian 500 and 1,000 rupee notes disappeared."),
              h5("The Government of India declared these notes invalid overnight."),
              h5("We are interested in looking at exports in India during this time.")
              
      ),
      
      # Second tab content
      tabItem(tabName = "exports",
              h2("India Top Export Partners"),
              box(
                title = "Please select year to view below.",
                selectInput("year", "Year:", choices = c(2014, 2015, 2016, 2017)),
                helpText("Data from United Nations COMTRADE Database.")
              )
      ),
      
      # Third tab content
      tabItem(tabName = "graphs",
              h2("Graphs")
      )
    )
  )
)

server <- function(input, output) { 

  output$my_table <- renderTable({
    head(iris, n = input$num)
  })  
  
  }

shinyApp(ui, server)