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
commodities <- read_rds("commodity_totals.rds")

# Define UI for the application
ui <- dashboardPage(
  dashboardHeader(title = "India's Exports"),
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
              h2("Welcome to the Indian international trade database!"),
              h6("For my final project, I was interested in understanding which countries
                 are most important for India's export economy. I provide graphs and tables
                 which show India's most important trading partners between 2014 and 2017."),
              h6("To explore this relationship, I downloaded data from the United Nations COMTRADE
                 database (https://comtrade.un.org/) and simplified and then merged the names of commodities using two-digit
                 classification codes."),
              h6("In 2017, the top export destinations of India were the United States
                 of America, United Arab Emirates, Hong Kong, China, and Singapore."),
              leafletOutput("mymap", height = "300")
              
      ),
      
      # Second tab content
      tabItem(tabName = "graphs",
              h2("India Top Export Partners"),
              box(
                title = "Please select year to view below.",
                radioButtons("year_choice", "Year:", 
                             choices = c(2014, 2015, 2016, 2017)), inline = TRUE
              ),
              plotOutput("distPlot2", height = 400)
      ),
      
      # Third tab content
      tabItem(tabName = "exports",
              h2("India Top Export Partners"),
              box(
                selectInput("year_choice", "Year:", 
                            choices = c(2014, 2015, 2016, 2017)),
                helpText("Data from United Nations COMTRADE Database.")
              ),
              DT::dataTableOutput("distPlot")
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
  
  datareact2 <- reactive({
    partners %>% 
      filter(year == input$year_choice) %>%
      filter(partner != "World") %>%
      arrange(desc(yearly_total)) %>%
      head(10)
  })
  
  output$distPlot <- DT::renderDataTable({
    datareact() %>%
      filter(partner != "World") %>%
      datatable(colnames = c('Year', 'Trade Partner', 'Trade Value (Billions USD)')) 

  })
  
  output$distPlot2 <- renderPlot({
    datareact2() %>%
    ggplot( 
           aes(x = reorder(partner, -yearly_total), 
               y = yearly_total)) + 
      geom_bar(stat="identity") + 
      geom_text(aes(label = yearly_total), vjust = 1.5, colour = "white", size = 3) +
      labs(x = NULL, y = "Trade Value (Billions USD)") + 
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
  })
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 78.0419, lat = 27.1750, zoom = 1.5) %>%
      addMarkers(lng = 78.0419, lat = 27.1750)
  })
    
  }

shinyApp(ui, server)