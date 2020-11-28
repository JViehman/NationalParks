library(shiny)
library(tidyverse)
library(httr)
library(jsonlite)
library(plyr)
library(scales)
library(lubridate)
library(dplyr)


load("nps.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Time Series of National Parks"),
    
    # Sidebar with a slider input for number of bins 
    selectInput("ParkName", "Park Name", unique(nps$fullName)),
    
    # Show a plot of the generated distribution
    mainPanel(
        plotOutput("distPlot")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    data <- reactive({
        req(input$ParkName)
        
        
        nps %>%
            filter(
                fullName == input$ParkName 
            )
        
    })
    
    
    output$distPlot <- renderPlot({
        ggplot(data(), aes(x=Date)) +
            geom_line(aes(y= log(RecreationVisits), color = "Recreation Visits"), size = .75) +
            geom_line(aes(y= log(Predicted), color = "Mars Predictions"), size = .75) +
            geom_line(aes(y= log(BoostPred), color = "XGBoost Predictions"), size = .75) +
            scale_x_date(breaks = "2 years", labels = date_format("%Y")) +
            scale_y_continuous(labels = comma) +
            theme_bw() + labs(y = "log(Visits)") + 
            scale_color_manual(values=c("Recreation Visits"= "black" ,"Mars Predictions" = "red", "XGBoost Predictions" = "purple"))
    })
    
    
}
# Run the application 
shinyApp(ui = ui, server = server)