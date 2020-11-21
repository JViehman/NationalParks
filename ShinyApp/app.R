library(shiny)
library(tidyverse)
library(httr)
library(jsonlite)
library(plyr)
library(scales)
library(lubridate)

nps1 = read.csv("NPS_master_Parks_1990to2019.csv")
nps = nps1[which(nps1$ParkName != "NA"),]
nps$fullName = paste(nps$ParkName, " ", nps$ParkType)

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
    
    nps1 = read.csv("NPS_master_Parks_1990to2019.csv")
    nps = nps1[which(nps1$ParkName != "NA"),]
    nps$fullName = paste(nps$ParkName, " ", nps$ParkType)
    nps$Date <- as.Date(nps$Date)
    
    output$distPlot <- renderPlot(ggplot(subset(nps, fullName %in% c(input$ParkName))) +
                        geom_line(mapping = aes(Date, RecreationVisits, color = ParkName), size = .75) +
                        scale_x_date(breaks = "2 years", labels = date_format("%Y")) +
                        scale_y_continuous(labels = comma) +
                        theme_bw())
    }

# Run the application 
shinyApp(ui = ui, server = server)
