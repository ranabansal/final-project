# This is the dashboard for the application
#

library(shiny)
library(tidyverse)
library(shinydashboard)
library(dplyr)
library(DT)
library(leaflet)
library(rgdal)

# Loading Chart Data
partners <- read_rds("top_partners_simp.rds")
qpal <- colorQuantile("Blues", NULL, n = 7)

# Define UI for the application
ui <- dashboardPage(
  dashboardHeader(title = "Indian Exports"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "dashboard", icon = icon("home")),
      menuItem("Top Partners by Year", tabName = "graphs", icon = icon("align-left")),
      menuItem("Partner Ranking (Table)", tabName = "exports", icon = icon("ship"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              h3("India is the 17th largest exporter in the world."),
              h5("Using this application, a user can identify India's most important
                 export partners and top exported commodities."),
              h5("In 2017, the top export destinations of India were the United States
                 of America, United Arab Emirates, Hong Kong, China, and Singapore."),
              leafletOutput("mymap", height = "300")
              
      ),
      
      # Second tab content
      tabItem(tabName = "exports",
              h2("India Top Export Partners"),
              box(
                title = "Please select year to view below.",
                selectInput("year_choice", "Year:", 
                            choices = c(2014, 2015, 2016, 2017)),
                helpText("Data from United Nations COMTRADE Database.")
              ),
              DT::dataTableOutput("distPlot")
      ),
      
      # Third tab content
      tabItem(tabName = "graphs",
              h2("India Top Export Partners"),
              box(
                title = "Please select year to view below.",
                selectInput("year_choice", "Year:", 
                            choices = c(2014, 2015, 2016, 2017)),
                helpText("Data from United Nations COMTRADE Database.")
              ),
              plotOutput("distPlot2")
      )
    )
  )
)

server <- function(input, output) { 
  
  datareact <- reactive({
    partners %>% 
      filter(year == input$year_choice) %>%
      arrange(desc(yearly_total))
  })
  
  output$distPlot <- DT::renderDataTable({
    datareact() %>%
      filter(partner != "World") %>%
      datatable(colnames = c('Year', 'Trade Partner', 'Trade Value (Billions USD)')) 

  })
  
  output$distPlot2 <- renderPlot({
    
  })
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 78.0419, lat = 27.1750, zoom = 1.75) %>%
      addMarkers(lng = 78.0419, lat = 27.1750, popup = "India")
  })
    
  }

shinyApp(ui, server)