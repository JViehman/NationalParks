###############################################################################
# Last Executed: 09/02/2020                                  
# File Name: Initial_NPS_Plots.R                               
# File Path: C:\Users\Beau\Google Drive (nfindley1@tamu.edu)\STAT 656 - Applied Analytics\Project\Scripts
# Created by: Beaumont Findley                               
# Created on: 09/02/2020                                     
# Purpose: Crate a series of plots
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

nps_parks <- read.csv("C:/Users/Beau/Google Drive (nfindley1@tamu.edu)/STAT 656 - Applied Analytics/Project/Data/NPS_Master_Parks.csv", header=T)
nps_parks_1990to2019 <- read.csv("C:/Users/Beau/Google Drive (nfindley1@tamu.edu)/STAT 656 - Applied Analytics/Project/Data/NPS_master_Parks_1990to2019.csv", header=T)
nps_parks_1990to2019$Date <- as.Date(nps_parks_1990to2019$Date)

###############################################################################
# Plots for the Park Metadata
###############################################################################

ggplot(subset(nps_parks, str_detect(States, "..,")==FALSE)) +
  geom_bar(mapping = aes(x=States)) +
  ylab("Number of NPS Parks") +
  theme_bw()
ggsave("CountParksByState.png", path = "C:/Users/Beau/Google Drive (nfindley1@tamu.edu)/STAT 656 - Applied Analytics/Project/Figures")

nps_parks_area <- nps_parks %>% 
                    arrange(States) %>%
                    dplyr::group_by(States) %>% 
                    dplyr::summarise(TotalAcres = sum(as.numeric(gsub(",", "", Gross.Area.Acres)), na.rm = TRUE))

ggplot(subset(nps_parks_area, str_detect(States, "..,")==FALSE)) +
  geom_bar(mapping = aes(x=States, y = TotalAcres), stat = "identity") +
  scale_y_continuous(name = "Total Park Size (Gross Area Acres)", labels = scales::comma) +
  theme_bw()
ggsave("ParkSizeGrossAcresByState.png", path = "C:/Users/Beau/Google Drive (nfindley1@tamu.edu)/STAT 656 - Applied Analytics/Project/Figures")

nps_parks_2019visits <- nps_parks %>% 
  arrange(States) %>%
  dplyr::group_by(States) %>% 
  dplyr::summarise(visits2019 = mean(as.numeric(gsub(",", "", AvgMonthlyVisits2019)), na.rm = TRUE))

ggplot(subset(nps_parks_2019visits, str_detect(States, "..,")==FALSE)) +
  geom_bar(mapping = aes(x=States, y = visits2019), stat = "identity") +
  scale_y_continuous(name = "2019 Average # of Park Visits", labels = scales::comma) +
  theme_bw()
ggsave("Average2019ParkVisitsByState.png", path = "C:/Users/Beau/Google Drive (nfindley1@tamu.edu)/STAT 656 - Applied Analytics/Project/Figures")

###############################################################################
# Plots for Individual Parks
###############################################################################

ggplot(subset(nps_parks_1990to2019, 
              UnitCode %in% c("GRSM"))) +
  geom_line(mapping = aes(Date, RecreationVisits, color = ParkName), size = .75) +
  scale_x_date(breaks = "2 years", labels = date_format("%Y")) +
  scale_y_continuous(labels = comma) +
  theme_bw()
ggsave("GreatSmokyMtns_Visitation.png", path = "C:/Users/Beau/Google Drive (nfindley1@tamu.edu)/STAT 656 - Applied Analytics/Project/Figures")

###############################################################################
# Plots for the Combined Park System
###############################################################################

ggplot(subset(nps_parks_1990to2019, Date>=as.Date("2000-01-01"))) +
  geom_point(mapping = aes(Date, RecreationVisits)) +
  scale_x_date(breaks = "1 years", labels = date_format("%Y"), minor_breaks = NULL) +
  scale_y_continuous(labels = comma) +
  theme_bw()
ggsave("CombinedVisitation.png", path = "C:/Users/Beau/Google Drive (nfindley1@tamu.edu)/STAT 656 - Applied Analytics/Project/Figures")
