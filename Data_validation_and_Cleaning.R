#Load Packages
library(tidyverse)
library(lubridate)
library(janitor)
library(data.table)
library(readr)
library(psych)
library(hrbrthemes)
library(ggplot2)

#Import Data
december_2021 <- read.csv("data/202112-divvy-tripdata.csv")
january_2022 <- read.csv("data/202201-divvy-tripdata.csv")
february_2022 <- read.csv("data/202202-divvy-tripdata.csv")
march_2022 <- read.csv("data/202203-divvy-tripdata.csv")
april_2022 <- read.csv("data/202204-divvy-tripdata.csv")
may_2022 <- read.csv("data/202205-divvy-tripdata.csv")
june_2022 <- read.csv("data/202206-divvy-tripdata.csv")
july_2022 <- read.csv("data/202207-divvy-tripdata.csv")
august_2022 <- read.csv("data/202208-divvy-tripdata.csv")
september_2022 <- read.csv("data/202209-divvy-publictripdata.csv")
october_2022 <- read.csv("data/202210-divvy-tripdata.csv")
november_2022 <- read.csv("data/202211-divvy-tripdata.csv")

#Data Validation
colnames(december_2021)
colnames(january_2022)
colnames(february_2022)
colnames(march_2022)
colnames(april_2022)
colnames(may_2022)
colnames(june_2022)
colnames(july_2022)
colnames(august_2022)
colnames(september_2022)
colnames(october_2022)
colnames(november_2022)

# Total number of rows
sum(nrow(december_2021) + nrow(january_2022) + nrow(february_2022) 
    + nrow(march_2022) + nrow(april_2022) + nrow(may_2022) 
    + nrow(june_2022) + nrow(july_2022) + nrow(august_2022)
    + nrow(september_2022) + nrow(october_2022) + nrow(november_2022))

# Combine Data of 12 month into for smooth workflow
trip_final <- rbind(december_2021,january_2022,february_2022,march_2022,april_2022,
                    may_2022,june_2022,july_2022,august_2022,september_2022,october_2022,november_2022)

# Save the combined files
write.csv(trip_final,file = "data/trip_final.csv",row.names = FALSE)

# Setting global variable size to inf
options(future.globals.maxSize = Inf)

#Final data validation
str(trip_final)
View(head(trip_final))
View(tail(trip_final))
dim(trip_final)
summary(trip_final)
names(trip_final)

#Data Cleaning

#Count rows with "na" values
colSums(is.na(trip_final))

#Remove missing
clean_trip_final <- trip_final[complete.cases(trip_final), ]
#Remove duplicates
clean_trip_final <- distinct(clean_trip_final)
#Remove data with greater start_at than end_at
clean_trip_final<- clean_trip_final %>% 
  filter(started_at < ended_at)
#Remove na
clean_trip_final <- drop_na(clean_trip_final)
clean_trip_final <- remove_empty(clean_trip_final)
clean_trip_final <- remove_missing(clean_trip_final)

#Check Cleaned data
colSums(is.na(clean_trip_final))
View(filter(clean_trip_final, clean_trip_final$started_at > clean_trip_final$ended_at))

#Renaming column for better context
clean_trip_final <- rename(clean_trip_final, costumer_type = member_casual, bike_type = rideable_type)

#Separate date in date, day, month, year for better analysis
clean_trip_final$date <- as.Date(clean_trip_final$started_at)
clean_trip_final$week_day <- format(as.Date(clean_trip_final$date), "%A")
clean_trip_final$month <- format(as.Date(clean_trip_final$date), "%b_%y")
clean_trip_final$year <- format(clean_trip_final$date, "%Y")

#Separate column for time
clean_trip_final$time <- as.POSIXct(clean_trip_final$started_at, format = "%Y-%m-%d %H:%M:%S")
clean_trip_final$time <- format(clean_trip_final$time, format = "%H:%M")

#Add ride length column
clean_trip_final$ride_length <- difftime(clean_trip_final$ended_at, clean_trip_final$started_at, units = "mins")

#Select the data we are going to use
clean_trip_final <- clean_trip_final %>% 
  select(bike_type, costumer_type, month, year, time, started_at, week_day, ride_length)

#Remove stolen bikes
clean_trip_final <- clean_trip_final[!clean_trip_final$ride_length>1440,] 
clean_trip_final <- clean_trip_final[!clean_trip_final$ride_length<5,] 

#Save the cleaned data
write.csv(clean_trip_final,file = "clean_trip_final.csv",row.names = FALSE)
