#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(scales)
alpha_two <- read.csv("Alphas2.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("India Exports 2014-2018"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("countries",
                  "Number of Countries:",
                  min = 1,
                  max = 16,
                  value = 16)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
# In here we are creating a plot (non-interactive for now) of imports to different countries from India.
# We need to filter out the fractions greater than 1 (don't make sense) and choose a specific commodity
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    alpha_two %>%
      filter(!Alpha > 1) %>%
      mutate(Month = as.Date(paste("01", Month,sep = "-"),"%d-%b-%y")) %>%
      filter(Commodity == "Iron and steel") %>%
      ggplot(aes(x = Month, y = Alpha, color = Import.Country)) + geom_point() +
      xlab("Date") +
      ylab("Fraction of Destination Country Imports from India") +
      ggtitle("Iron and Steel") 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

