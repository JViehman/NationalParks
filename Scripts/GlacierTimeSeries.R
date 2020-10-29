###############################################################################
# Last Executed: 10/28/2020                                  
# File Name: GlacierTimeSeries.R                               
# File Path: C:\Users\Beau\Google Drive (nfindley1@tamu.edu)\STAT 656 - Applied Analytics\Project\national_parks\Scripts
# Created by: Beaumont Findley                               
# Created on: 10/28/2020                                     
# Purpose: Create a Univariate Time Series and plot for Glacier NP
###############################################################################

###############################################################################
# Set up working environment
###############################################################################

library(httr)
library(jsonlite)
library(plyr)
library(tidyverse)
library(scales)
library(lubridate)
library(forecast)
library(scales)

funggcast<-function(dn,fcast){ 
  require(zoo) #needed for the 'as.yearmon()' function
  
  en<-max(time(fcast$mean)) #extract the max date used in the forecast
  
  #Extract Source and Training Data
  ds<-as.data.frame(window(dn, end=en))
  names(ds)<-'observed'
  ds$date<-as.Date(time(window(dn, end=en)))
  
  #Extract the Fitted Values (need to figure out how to grab confidence intervals)
  dfit<-as.data.frame(fcast$fitted)
  dfit$date<-as.Date(time(fcast$fitted))
  names(dfit)[1]<-'fitted'
  
  ds<-merge(ds, dfit, all.x=T) #Merge fitted values with source and training data
  
  #Exract the Forecast values and confidence intervals
  dfcastn<-as.data.frame(fcast)
  dfcastn$date<-as.Date(as.yearmon(row.names(dfcastn)))
  names(dfcastn)<-c('forecast','lo80','hi80','lo95','hi95','date')
  
  pd<-merge(ds,dfcastn, all.x=T) #final data.frame for use in ggplot
  return(pd)
}

nps_parks <- read.csv("C:/Users/Beau/Google Drive (nfindley1@tamu.edu)/STAT 656 - Applied Analytics/Project/national_parks/Data Files/NPS_Master_Parks.csv", header=T)
nps_parks_1990to2019 <- read.csv("C:/Users/Beau/Google Drive (nfindley1@tamu.edu)/STAT 656 - Applied Analytics/Project/national_parks/Data Files/NPS_master_Parks_1990to2019.csv", header=T)
nps_parks_1990to2019$Date <- as.Date(nps_parks_1990to2019$Date)

###############################################################################
# Plots for Individual Parks
###############################################################################

glac = subset(nps_parks_1990to2019, UnitCode %in% c("GLAC") & Date>=as.Date("2015-01-01")) %>% select(RecreationVisits)
series = ts(data = glac, frequency = 12, start = c(2015,1,1))

seriest<-window(series, end=2018.99)
seriesfit<-auto.arima(y = seriest, start.p = 0, max.p = 3, start.q = 0, max.q = 3)
seriesfor<-forecast(seriesfit)
pd<-funggcast(series, seriesfor)

ggplot(data = pd, mapping = aes(date, observed)) +
  geom_line(col = 'lightblue', size = 1.5, alpha = .5) +
  geom_line(aes(y=fitted), col = 'dark red', size = 1) +
  geom_line(aes(y=forecast), col = 'black')+
  geom_ribbon(aes(ymin=lo95,ymax=hi95), alpha=.25)+
  scale_x_date(breaks = "2 years", labels = date_format("%Y")) +
  scale_y_continuous(labels = comma) +
  theme_bw()
#ggsave("Glacier_Visitation.png", path = "C:/Users/Beau/Google Drive (nfindley1@tamu.edu)/STAT 656 - Applied Analytics/Project/Figures")

series_decomp = decompose(series, type = "multiplicative")
plot(series_decomp)

(rmse = sqrt(mean((pd$observed-pd$forecast)^2, na.rm = TRUE)))

series_mod = auto.arima(y = series, start.p = 0, max.p = 3, start.q = 0, max.q = 3)
autoplot(forecast(series_mod))
