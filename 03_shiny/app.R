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
library(ggplot2)

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

        ggplot(data=slovenia_filtered, aes(x=temperature)) +
            geom_histogram(color="white", size=1, bins=input$bins, na.rm=TRUE) +
            xlim(-25, 50) +
            ggtitle("Slovenia") +
            theme_minimal()
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
        
        ggplot(data=finland_filtered, aes(x=temperature)) +
            geom_histogram(color="white", size=1, bins=input$bins, na.rm=TRUE) +
            xlim(-25, 50) +
            ggtitle("Finland") +
            theme_minimal()
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

        ggplot(data=niger_filtered, aes(x=temperature)) +
            geom_histogram(color="white", size=1, bins=input$bins, na.rm=TRUE) +
            xlim(-25, 50) +
            ggtitle("Niger") +
            theme_minimal()
    })   
}

# Run the application 
shinyApp(ui = ui, server = server)
