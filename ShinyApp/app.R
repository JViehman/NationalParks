library(shiny)
library(tidyverse)
library(httr)
library(jsonlite)
library(plyr)
library(scales)
library(lubridate)
library(dplyr)

#nps1 = load("~/ibm/NaitonalParks/testdata.RData")
#nps1 = combined
#nps = nps1[which(nps1$ParkName != "NA"),]
#nps$fullName = paste(nps$ParkName, " ", nps$ParkType)
#nps$RecreationVisits = exp(nps$Y)
#nps$Predicted = exp(nps$MarsPred)
#nps$date <- ifelse(substr(nps$MonthName, 1, 3) %in% month.abb,
#            paste(match(substr(nps$MonthName, 1, 3), month.abb),
#                  1,
#                  nps$Year, sep = "/"), NA)
#nps$Date <-  as.Date(nps$date, format = "%m/%d/%Y")
#save(nps, file="~/ibm/nps.RData")
#load("~/ibm/nps.RData")
#load("~/ibm/XGBcombined.RData")
#xg <- combined[c("BoostPred", "Y", "logAcres", "Age", "fees", "publicAcres", "Year")]

#data <- merge(nps, xg, by=c("Y", "logAcres", "Age", "fees", "publicAcres", "Year")) 
#data$BoostPred <- exp(data$BoostPred)
#nps <- data
#save(nps, file="~/ibm/nps.RData")
load("~/ibm/nps.RData")

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
            geom_line(aes(y= log(RecreationVisits)), color = "black", size = .75) +
            geom_line(aes(y= log(Predicted)), color = "red", size = .75) +
            geom_line(aes(y= log(BoostPred)), color = "purple", size = .75) +
            scale_x_date(breaks = "2 years", labels = date_format("%Y")) +
            scale_y_continuous(labels = comma) +
            theme_bw() + labs(y = "log(Visits)", caption = "MARS Predicted Values in Red, Actual Values in Black, XGBoost predicted values in purple") + 
            scale_color_manual(values=c("black"= "Actual","red" = "Predicted"))
    })
    
    
}
# Run the application 
shinyApp(ui = ui, server = server)