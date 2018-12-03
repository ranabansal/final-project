# This is the dashboard for the application
#

library(shiny)
library(tidyverse)
library(shinydashboard)
library(dplyr)
library(DT)
library(leaflet)
library(rgdal)
library(scales)

# Loading Chart Data
partners <- read_rds("top_partners_simp.rds")
qpal <- colorQuantile("Blues", NULL, n = 7)
commodities <- read_rds("commodity_totals.rds")
alpha_two <- read.csv("Alphas2.csv")
alpha_two <- alpha_two %>%
  mutate(newcol = case_when(Import.Country == "United States of America" ~ 1,
                            Import.Country == "United Arab Emirates" ~ 2,
                            Import.Country == "China, Hong Kong SAR" ~ 3,
                            Import.Country == "China" ~ 4,
                            Import.Country == "Singapore" ~ 5,
                            Import.Country == "United Kingdom" ~ 6,
                            Import.Country == "Germany" ~ 7,
                            Import.Country == "Netherlands" ~ 8,
                            Import.Country == "Belgium" ~ 9,
                            Import.Country == "Japan" ~ 10,
                            Import.Country == "South Africa" ~ 11,
                            Import.Country == "Turkey" ~ 12,
                            Import.Country == "Italy" ~ 13,
                            Import.Country == "France" ~ 14,
                            Import.Country == "Malaysia" ~ 15,
                            Import.Country == "Spain" ~ 16))

# Define UI for the application
ui <- dashboardPage(
  dashboardHeader(title = "India's Exports"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "dashboard", icon = icon("home")),
      menuItem("Top Partners by Year", tabName = "graphs", icon = icon("align-left")),
      menuItem("Partner Ranking (Table)", tabName = "exports", icon = icon("ship")),
      menuItem("Top Commodities by Year", tabName = "commodities", icon = icon("balance-scale")),
      menuItem("Iron and Steel Snapshot", tabName = "snapshot", icon = icon("camera"))
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
                selectInput("year_choice2", "Year:", 
                            choices = c(2014, 2015, 2016, 2017)),
                helpText("Data from United Nations COMTRADE Database.")
              ),
              DT::dataTableOutput("distPlot")
      ),
      
      tabItem(tabName = "commodities",
              h2("India Top Commodities"),
              box(
                selectInput("year_choice3", "Year:",
                            choices = c(2014, 2015, 2016, 2017)),
                helpText("Commodities sorted by two-digit HS international code.")
              ),
              DT::dataTableOutput("commodityPlot")
      ),
      
      tabItem(tabName = "snapshot",
              h2("Iron and Steel Exports"),
              box(
                sliderInput("countries",
                            "Number of Countries:",
                            min = 1,
                            max = 15,
                            value = 10)
              ),
              plotOutput("complot1", height = 400)
      )
    )
  )
)

server <- function(input, output) { 
  
  datareact <- reactive({
    partners %>% 
      filter(year == input$year_choice2) %>%
      arrange(desc(yearly_total))
  })
  
  datareact2 <- reactive({
    partners %>% 
      filter(year == input$year_choice) %>%
      filter(partner != "World") %>%
      arrange(desc(yearly_total)) %>%
      head(10)
  })
  
  datareact3 <- reactive({
    commodities %>% 
      filter(year == input$year_choice3) %>%
      arrange(desc(commodity_total)) %>%
      head(15)
  })
  
  datareact4 <- reactive({
    alpha_two %>%
      filter(newcol <= input$countries)
  })
  
  output$distPlot <- DT::renderDataTable({
    datareact() %>%
      filter(partner != "World") %>%
      datatable(colnames = c('Year', 'Trade Partner', 'Trade Value (Billions USD)'))
  })
  
  output$commodityPlot <- DT::renderDataTable({
    datareact3() %>%
      datatable(colnames = c('Year', 'Commodity', 'Name', 'Total Trade Value (USD)')) 
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
  
  output$complot1 <- renderPlot({
    datareact4() %>%
      filter(!Alpha > 1) %>%
      mutate(Month = as.Date(paste("01", Month,sep = "-"),"%d-%b-%y")) %>%
      filter(Commodity == "Iron and steel") %>%
      ggplot(aes(x = Month, y = Alpha, color = Import.Country)) + geom_point() +
      xlab("Date") +
      ylab("Fraction of Destination Country Imports from India") +
      ggtitle("Iron and Steel") +
      labs(caption = "Export Data from United Nations COMTRADE Database")
  })
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 78.0419, lat = 27.1750, zoom = 1.5) %>%
      addMarkers(lng = 78.0419, lat = 27.1750)
  })
    
  }

shinyApp(ui, server)