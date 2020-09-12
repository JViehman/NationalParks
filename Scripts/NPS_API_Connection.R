###############################################################################
# Last Executed: 09/02/2020                                  
# File Name: NPS_API_Connection.R                               
# File Path: C:\Users\Beau\Google Drive (nfindley1@tamu.edu)\STAT 656 - Applied Analytics\Project\Scripts
# Created by: Beaumont Findley                               
# Created on: 08/25/2020                                     
# Purpose: Crate a dataset for use in forecasting visitation of US national parks.
#     The dataset will have 1 line for each month between 1990 and 2019 for each park.
###############################################################################


###############################################################################
# Set up working environment
###############################################################################

library(httr)
library(jsonlite)
library(tidyverse)
library(plyr)
library(scales)
library(lubridate)

options(stringsAsFactors = FALSE)

###############################################################################
# Connect to the NPS API to pull down park metadata by state (limit of 50 obs per pull so multiple pulls are needed)
###############################################################################

states = c(state.abb, "DC", "GU", "AS", "MP", "PR", "VI")

nps_api_parks <- function(state=NULL){
  if(!require(tidyverse)){install.packages('tidyverse');require(tidyverse)}
  nps_parks <- list()
  for (i in 1:length(state)) {
      # Use the NPS API to get JSON data for the elements of the state vector sequentially
      resp <- GET("https://developer.nps.gov/api/v1/parks?api_key=8zBBMjF9dd5qgTFaosUiU7fhkh8mI9gVo0MDSdDk", query = list(stateCode = state[i]))
      raw <- fromJSON(rawToChar(resp$content))$data
      # Restructure the JSON activities column
      raw$activities <- sapply(raw$activities, function(x) ifelse(nrow(x[0])==0, NA, x[2]))
      raw$activities <- sapply(raw$activities, function(x) paste0(x, collapse = ", "))
      # Restructure the JSON topics column
      raw$topics <- sapply(raw$topics, function(x) ifelse(nrow(x[0])==0, NA, x[2]))
      raw$topics <- sapply(raw$topics, function(x) paste0(x, collapse = ", "))
      # Restructure the JSON cost column
      raw$entranceFees = unlist(flatten(lapply(raw$entranceFees, function(x) ifelse(nrow(x[0])==0, NA, paste0(x[[3]], " (", x[[1]], ")", collapse = ", ")))))
      # Restructure the JSON Zip Code column
      raw$City = unlist(lapply(raw$addresses, function(x) ifelse(nrow(x[0])==0, NA, x[[2]][1])))
      raw$ZipCode = unlist(flatten(lapply(raw$addresses, function(x) ifelse(nrow(x[0])==0, NA, x[[1]][1]))))
      # Iteratively add on to the dataset
      nps_parks[[states[i]]] <- as_tibble(raw) %>% select(id, name, fullName, designation, parkCode, states, City, ZipCode, latitude, longitude, activities, topics, entranceFees)
  }
  return(dplyr::bind_rows(nps_parks) %>% distinct(fullName, .keep_all = TRUE))
}

nps_parks <- nps_api_parks(states)
nps_parks$UnitCode <- toupper(nps_parks$parkCode)

###############################################################################
# Add on the Acreage data to the complete NPS park metadata file
###############################################################################

nps_acreage <- read.csv("C:/Users/Beau/Google Drive (nfindley1@tamu.edu)/STAT 656 - Applied Analytics/Project/Data/Source_Data/NPS_Acrage_2019.csv", header = T, strip.white = T)

nps_acreage <- nps_acreage %>% 
                  select(UnitCode,NPS.Fee.Acres, Private.Acres, Gross.Area.Acres) %>%
                  filter(UnitCode != "") %>%
                  arrange(UnitCode)

###############################################################################
# Add on the Age data to the complete NPS park metadata file
###############################################################################

nps_park_ages <- read.csv("C:/Users/Beau/Google Drive (nfindley1@tamu.edu)/STAT 656 - Applied Analytics/Project/Data/Source_Data/NPS_Park_Ages.csv", header = T, strip.white = T)
nps_park_ages <- nps_park_ages %>% 
                    mutate(Day = word(Date, 1, sep = "\\-"),
                           Month = word(Date, 2, sep = "\\-"),
                           DateCreated = ymd(paste(Year, Month, Day, sep = "/")),
                           Age = ceiling(difftime(Sys.Date(), DateCreated, units = "weeks")/52.25)) %>%
                    select(UnitCode, DateCreated, Age) %>%
                    arrange(UnitCode) %>%
                    filter(UnitCode != "")

###############################################################################
# Create a dataset for the park visitation values
###############################################################################

paths = dir("C:/Users/Beau/Google Drive (nfindley1@tamu.edu)/STAT 656 - Applied Analytics/Project/Data/Source_Data",
            pattern="NPS_Traffic_", full.names=TRUE)

df_list = lapply(paths,read.csv)
nps_visitation     = do.call(rbind, df_list)

nps_visitation <- nps_visitation %>%
  mutate(ParkName = ï..ParkName, MonthName = month.name[Month], Day = 1, DateS = paste(Year, Month, Day, sep = "-"),
         Date = as.Date(DateS),
         AvgMonthlyVisits = as.numeric(gsub(",", "", RecreationVisitsTotal))/12, 
         AvgMonthlyHours = as.numeric(gsub(",", "", RecreationHoursTotal))/12) %>%
  arrange(ParkName)

nps_visitation <- nps_visitation %>%
  select(ParkName, UnitCode, ParkType, Region, State, Date, Year, Month, MonthName, RecreationVisits,
         RecreationHours, RecreationVisitsTotal, RecreationHoursTotal, AvgMonthlyVisits, AvgMonthlyHours)

###############################################################################
# Create a master metadata file for all parks
###############################################################################

nps_visitation2019 <- nps_visitation %>% 
                        filter(Date=="2019-01-01")%>%
                        select(UnitCode, Region, RecreationVisitsTotal2019 = RecreationVisitsTotal, 
                               RecreationHoursTotal2019 = RecreationHoursTotal, 
                               AvgMonthlyVisits2019 = AvgMonthlyVisits, 
                               AvgMonthlyHours2019 = AvgMonthlyHours)

nps_parks <- left_join(nps_parks, nps_acreage, by = "UnitCode")
nps_parks <- left_join(nps_parks, nps_park_ages, by = "UnitCode")
nps_parks <- left_join(nps_parks, nps_visitation2019, by = "UnitCode")

nps_parks <- nps_parks %>%
                mutate(NPFlag = ifelse(str_detect(designation, "National Park$|National Park |National Parks")==TRUE, 1, 0)) %>%
                select(ParkName = name, UnitCode, ParkType = designation, NPFlag, Region, States = states, latitude, longitude, DateCreated, Age, 
                       NPS.Fee.Acres, Private.Acres, Gross.Area.Acres, 
                       RecreationVisitsTotal2019, RecreationHoursTotal2019, AvgMonthlyVisits2019, AvgMonthlyHours2019,
                       activities, topics, entranceFees) %>%
                arrange(UnitCode)

write.csv(nps_parks, file = "C:/Users/Beau/Google Drive (nfindley1@tamu.edu)/STAT 656 - Applied Analytics/Project/Data/NPS_Master_Parks.csv", row.names = F)

###############################################################################
# Create an expanded master metadata file for all parks
###############################################################################

nps_parks_expanded <- nps_parks

nps_parks_activities = unique(unlist(lapply(str_split(nps_parks$activities, ", "), function(x) c(x))))
nps_parks_topics = unique(unlist(lapply(str_split(nps_parks$topics, ", "), function(x) c(x))))

nps_parks_activities <- nps_parks_activities[!is.na(nps_parks_activities)]
nps_parks_topics <- nps_parks_activities[!is.na(nps_parks_activities)]

for (i in 1:length(nps_parks_activities)) {
  nps_parks_expanded[, paste("Act_", nps_parks_activities, sep = "")] <- ifelse(str_detect(nps_parks_expanded$activities, nps_parks_activities[i]) == TRUE, 1, 0)
  nps_parks_expanded[, paste("Top_", nps_parks_topics, sep = "")] <- ifelse(str_detect(nps_parks_expanded$topics, nps_parks_topics[i]) == TRUE, 1, 0)
}

write.csv(nps_parks_expanded, file = "C:/Users/Beau/Google Drive (nfindley1@tamu.edu)/STAT 656 - Applied Analytics/Project/Data/NPS_Master_Parks_Expanded.csv", row.names = F)

###############################################################################
# Join the visitation data with the park metadata
###############################################################################

nps_parks_1990to2019 <- left_join(nps_visitation, nps_parks, by = "UnitCode")

nps_parks_1990to2019 <- nps_parks_1990to2019 %>%
                          mutate(NPS.Fee.Acres = as.numeric(gsub(",", "", NPS.Fee.Acres)), 
                                 Private.Acres = as.numeric(gsub(",", "", Private.Acres)),
                                 Gross.Area.Acres = as.numeric(gsub(",", "", Gross.Area.Acres)),
                                 RecreationVisits = as.numeric(gsub(",", "", RecreationVisits)),
                                 RecreationHours = as.numeric(gsub(",", "", RecreationHours)))

nps_parks_1990to2019 <- nps_parks_1990to2019 %>%
  select(ParkName = ParkName.y, UnitCode, ParkType = ParkType.y, NPFlag, Region = Region.x, States, latitude, longitude, DateCreated, Age, 
         NPS.Fee.Acres, Private.Acres, Gross.Area.Acres, Date, Year, Month, MonthName,
         RecreationVisits, RecreationHours, activities, topics, entranceFees) %>%
  arrange(ParkName)

write.csv(nps_parks_1990to2019, file = "C:/Users/Beau/Google Drive (nfindley1@tamu.edu)/STAT 656 - Applied Analytics/Project/Data/NPS_master_Parks_1990to2019.csv", row.names = F)




