#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)

data <- read.csv("../data/temperature.csv", sep=";")
data_slovenia <- data %>% filter(data$country == "Slovenia")
data_finland <- data %>% filter(data$country == "Finland")
data_niger <- data %>% filter(data$country == "Niger")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Temperature comparison"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 100,
                        value = 50),
            
            sliderInput("year",
                        "Years:",
                        step = 1,
                        min = min(data$year),
                        max = max(data$year),
                        value = c(min(data$year), max(data$year))),
            
            sliderInput("month",
                        "Months:",
                        step = 1,
                        min = min(data$month),
                        max = max(data$month),
                        value = c(min(data$month), max(data$month)))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("slovenia_plot", height="200px"),
           plotOutput("finland_plot", height="200px"),
           plotOutput("niger_plot", height="200px")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$slovenia_plot <- renderPlot({
        # year
        min_year <- input$year[1]
        max_year <- input$year[2]
        
        # month
        min_month <- input$month[1]
        max_month <- input$month[2]
        
        filtered_data <- data %>%
            filter(year <= max_year & year >= min_year) %>%
            filter(month <= max_month & month >= min_month)
        
        slovenia_filtered <- filtered_data %>% filter(country == "Slovenia")
        x <- slovenia_filtered$temperature
        
        # generate bins based on input$bins from ui.R
        bins <- seq(-25, 50, length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x,
             breaks = bins,
             col = 'darkgray',
             border = 'white',
             main = "Slovenia")
    })
    
    output$finland_plot <- renderPlot({
        # year
        min_year <- input$year[1]
        max_year <- input$year[2]
        
        # month
        min_month <- input$month[1]
        max_month <- input$month[2]
        
        filtered_data <- data %>%
            filter(year <= max_year & year >= min_year) %>%
            filter(month <= max_month & month >= min_month)
        
        finland_filtered <- filtered_data %>% filter(country == "Finland")
        x <- finland_filtered$temperature
        
        # generate bins based on input$bins from ui.R
        bins <- seq(-25, 50, length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x,
             breaks = bins,
             col = 'darkgray',
             border = 'white',
             main = "Finland")
    })    
 
    output$niger_plot <- renderPlot({
        # year
        min_year <- input$year[1]
        max_year <- input$year[2]
        
        # month
        min_month <- input$month[1]
        max_month <- input$month[2]
        
        filtered_data <- data %>%
            filter(year <= max_year & year >= min_year) %>%
            filter(month <= max_month & month >= min_month)
        
        niger_filtered <- filtered_data %>% filter(country == "Niger")
        x <- niger_filtered$temperature
        
        # generate bins based on input$bins from ui.R
        bins <- seq(-25, 50, length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x,
             breaks = bins,
             col = 'darkgray',
             border = 'white',
             main = "Niger")
    })   
}

# Run the application 
shinyApp(ui = ui, server = server)
