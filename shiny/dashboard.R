# This is the dashboard for the application, which houses four tabs.
# Each of the four tabs includes different information on India's export sector.

# Here, we load the necessary libraries that will be used in the application.
library(shiny)
library(tidyverse)
library(shinydashboard)
library(dplyr)
library(DT)
library(leaflet)
library(rgdal)
library(scales)

# Here, we load the datasets that will be used for the dashboard.
# This data has all the top partners for each year to be used in table and graph.
partners <- read_rds("top_partners_simp.rds")

# This data has information on most frequently exported commodities by year. 
commodities <- read_rds("commodity_totals.rds")

# This data has information on what fraction of imports India takes up
# For a specific commodity in a specific country
alpha_two <- read.csv("Alphas2.csv")

# Here, we add a new column that assigns a rank to each country based on how important
# the country is as an export partner.
# Later in the code, we use this rank to decide whether or not to show country on the graph. 
# This is important for the numerical slider input. 
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
  # The overall title of the dashboard 
  dashboardHeader(title = "India's Exports"),
  dashboardSidebar(
    # There are a total of four main tabs, followed by a snapshot for a commodity. 
    # I added interesting icons to match the topics of the tabs. 
    sidebarMenu(
      menuItem("Home", tabName = "dashboard", icon = icon("home")),
      menuItem("Top Partners by Year", tabName = "graphs", icon = icon("align-left")),
      menuItem("Partner Ranking (Table)", tabName = "exports", icon = icon("ship")),
      menuItem("Top Commodities by Year", tabName = "commodities", icon = icon("balance-scale")),
      menuItem("Iron and Steel Snapshot", tabName = "snapshot", icon = icon("cogs")),
      menuItem("Coffee and Tea Snapshot", tabName = "snapshot2", icon = icon("coffee"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      # This provides information on the contents of the app.
      # Also, provides info on data resource and some basic summary stats on trade.
      # Finally, just for the effect, I included a graph on the page that looks nice.
      # Including a graph is nice as we are looking at international trade data. 
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
      # On this tab, the user chooses which year they want to see.
      # After selecting the year, they see a bar graph of the top India export partners.
      # There is a select height for the graph. 
      tabItem(tabName = "graphs",
              h2("India Top Export Partners"),
              box(
                title = "Please select year to view below.",
                radioButtons("year_choice", "Year:", 
                             choices = c(2014, 2015, 2016, 2017)), inline = TRUE
              ),
              plotOutput("distPlot2", height = 275)
      ),
      
      # Third tab content
      # This tab includes the information from the first tab (top exporters),
      # however, it has a function where people can search by country name.
      # Also, unlike the previous tab which only shows top, this continues the list. 
      tabItem(tabName = "exports",
              h2("India Top Export Partners"),
              box(
                selectInput("year_choice2", "Year:", 
                            choices = c(2014, 2015, 2016, 2017)),
                helpText("Data from United Nations COMTRADE Database.")
              ),
              DT::dataTableOutput("distPlot")
      ),
      
      # Fourth tab content
      # This tab allows users to see the top exported commodity items by year
      # This requires a new dataset with merged HS commodity names (see analysis file)
      tabItem(tabName = "commodities",
              h2("India Top Commodities"),
              box(
                selectInput("year_choice3", "Year:",
                            choices = c(2014, 2015, 2016, 2017)),
                helpText("Commodities sorted by two-digit HS international code.")
              ),
              DT::dataTableOutput("commodityPlot")
      ),
      
      # Fifth tab content
      # This tab also requires a new dataset, specifically one on India's fraction of exports.
      # It provides detailed information on the fraction of each country's iron/steel exports
      # that are provided by India, and how this fraction changed over time. 
      # By playing with the slider, users can change how many of the top export partners are chosen.
      tabItem(tabName = "snapshot",
              h4("Iron and Steel Exports"),
              h6("The graph below plots the share India has for a given country's imports
                 of iron and steel, for each month between January 2014 and December 2017.
                 The higher the fraction, the more important India's exports are for that country
                 in the given month period. The user can select how many countries he or she
                 would like to see."), 
              h6("I chose this commodity good because iron and steel are relatively homogenous goods
                 which these countries can get from a variety of sources, so looking at how much they choose
                 to get from India over time is informative of the quality of relationship."),
              box(
                sliderInput("countries",
                            "Number of Countries:",
                            min = 1,
                            max = 15,
                            value = 10)
              ),
              plotOutput("complot1", height = "300")
      ),
      
      # We provide another snapshot of a different commodity, this time coffee/tea
      # We provide some background info on the good.
      # The functionality is the same. 
      tabItem(tabName = "snapshot2",
              h4("Coffee and Tea Exports"),
              h6("The graph below plots the share India has for a given country's imports
                 of coffee, tea, mate, and spices for each month between January 2014 and December 2017.
                 The higher the fraction, the more important India's exports are for that country
                 in the given month period. The user can select how many countries he or she
                 would like to see."),
              h6("I chose this commodity good as India is a large exporter of tea (famous for Darjeeling Tea)
                 and I was interested in which countries rely on India for breakfast!"),
              box(
                sliderInput("countries2",
                            "Number of Countries:",
                            min = 1,
                            max = 15,
                            value = 10)
              ),
              plotOutput("complot2", height = "300")
      )
    )
  )
)

# This is the server component of the app, which takes the inputs into account. 
server <- function(input, output) { 
  
  # The user chooses a year from the selection and the partners are ranked for that year.
  datareact <- reactive({
    partners %>% 
      filter(year == input$year_choice2) %>%
      arrange(desc(yearly_total))
  })
  
  # Like above, user chooses a year and the partners are ranked. 
  # There is no interest in seeing whole world, only by country .
  # Only the top 10 partners are relevant for the graph. 
  datareact2 <- reactive({
    partners %>% 
      filter(year == input$year_choice) %>%
      filter(partner != "World") %>%
      arrange(desc(yearly_total)) %>%
      head(10)
  })
  
  # Like above, user chooses a year and now the commodities are ranked in order.
  # The order is size of total commodity exports in billions
  datareact3 <- reactive({
    commodities %>% 
      filter(year == input$year_choice3) %>%
      arrange(desc(commodity_total))
  })
  
  # Here, the user selects how many countries they would like to see.
  # We ranked the countries above in the variable newcol.
  # Here, you select only a certain number of the countries (top x number)
  datareact4 <- reactive({
    alpha_two %>%
      filter(newcol <= input$countries)
  })
  
  # Same functionality as above for coffee and tea exports
  datareact5 <- reactive({
    alpha_two %>%
      filter(newcol <= input$countries2)
  })
  
  # We do not want to show the "World" partner only the individual countries. 
  # We also add appropriate column names. 
  output$distPlot <- DT::renderDataTable({
    datareact() %>%
      filter(partner != "World") %>%
      datatable(colnames = c('Year', 'Trade Partner', 'Trade Value (Billions USD)'))
  })
  
  # We add appropriate names for column of table.
  # We include the commodity code and simplified name for reference. 
  output$commodityPlot <- DT::renderDataTable({
    datareact3() %>%
      datatable(colnames = c('Year', 'Commodity', 'Name', 'Trade Value (Billions USD)')) 
  })
  
  # This is the barchart for the first tab of the graph
  # We choose a color matching India's flag colors orange and green
  # We make some adjustments to make the labels of the data and bars readable
  # We also remove the gridlines to make the graph better looking. 
  output$distPlot2 <- renderPlot({
    datareact2() %>%
    ggplot( 
           aes(x = reorder(partner, -yearly_total), 
               y = yearly_total)) + 
      geom_bar(stat="identity", color = "seashell", fill = "orangered2") + 
      geom_text(aes(label = yearly_total), vjust = 1.5, colour = "white", size = 3) +
      labs(x = NULL, y = "Trade Value (Billions USD)") + 
      theme(axis.text.x = element_text(angle = 60, hjust = 1), 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) 
  })
  
  # This is the graph for the snapshot of iron and steel exports
  # We are only looking at the commodity for Iron and Steel
  # We add labels for the x and y axis that are necessary.
  output$complot1 <- renderPlot({
    datareact4() %>%
      filter(!Alpha > 1) %>%
      mutate(Month = as.Date(paste("01", Month,sep = "-"),"%d-%b-%y")) %>%
      filter(Commodity == "Iron and steel") %>%
      ggplot(aes(x = Month, y = Alpha, color = Import.Country)) + geom_point() +
      xlab("Date") +
      ylab("Fraction of Destination Country Imports from India") +
      ggtitle("Iron and Steel") +
      labs(caption = "Export Data from United Nations COMTRADE Database") +
      scale_colour_discrete(name = "Import Country")
  })
  
  # This is the graph for the snapshot of coffee/tea exports
  # We are only looking at the commodity for Coffee and tea
  # We add labels for the x and y axis that are necessary.
  # We use the same formatting from the previous page.
  output$complot2 <- renderPlot({
    datareact5() %>%
      filter(!Alpha > 1) %>%
      mutate(Month = as.Date(paste("01", Month,sep = "-"),"%d-%b-%y")) %>%
      filter(Commodity == "Coffee, tea, mate and spices") %>%
      ggplot(aes(x = Month, y = Alpha, color = Import.Country)) + geom_point() +
      xlab("Date") +
      ylab("Fraction of Destination Country Imports from India") +
      ggtitle("Coffee, tea, mate and spices") +
      labs(caption = "Export Data from United Nations COMTRADE Database") +
      scale_colour_discrete(name = "Import Country")
  })
  
  # This map is just for aesthetic effect for the dashboard.
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 78.0419, lat = 27.1750, zoom = 3)
  })
    
  }

shinyApp(ui, server)