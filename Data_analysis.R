#Load Packages
library(tidyverse)
library(lubridate)
library(janitor)
library(data.table)
library(readr)
library(psych)
library(hrbrthemes)
library(ggplot2)

#import the cleaned data
clean_trip_final <- read_csv("clean_trip_final.csv")
str(clean_trip_final)
names(clean_trip_final)

#order the data
clean_trip_final$month <- ordered(clean_trip_final$month,levels=c("Dec_21","Jan_22","Feb_22","Mar_22", 
                                                                   "Apr_22","May_22","Jun_22","Jul_22", 
                                                                   "Aug_22","Sep_22","Oct_22","Nov_22"))

clean_trip_final$week_day <- ordered(clean_trip_final$week_day, levels = c("Sunday", "Monday", "Tuesday", 
                                                                           "Wednesday", "Thursday", 
                                                                           "Friday", "Saturday"))

#Analysis:- min, max, median, average
View(describe(clean_trip_final$ride_length, fast=TRUE))

#Total no. of customers
View(table(clean_trip_final$costumer_type))

#Total rides for each customer type in minutes
View(setNames(aggregate(ride_length ~ costumer_type, clean_trip_final, sum), c("customer_type", "total_ride_len(mins)")))

#Differences between members and casual riders in terms of length of ride
View(clean_trip_final %>% 
       group_by(costumer_type) %>% 
       summarise(min_length_mins = min(ride_length), max_length_min = max(ride_length),
                 median_length_mins = median(ride_length), mean_length_min = mean(ride_length)))

#Average ride_length for users by day_of_week and Number of total rides by day_of_week
View(clean_trip_final %>% 
       group_by(week_day) %>% 
       summarise(Avg_length = mean(ride_length),
                 number_of_ride = n()))

#Average ride_length by month
View(clean_trip_final %>% 
       group_by(month) %>% 
       summarise(Avg_length = mean(ride_length),
                 number_of_ride = n()))

#Average ride length comparison by each week day according to each customer type
View(aggregate(clean_trip_final$ride_length ~ clean_trip_final$costumer_type + 
                 clean_trip_final$week_day, FUN = mean))

#Average ride length comparison by each month according to each customer type
View(aggregate(clean_trip_final$ride_length ~ clean_trip_final$costumer_type + 
                 clean_trip_final$month, FUN = mean))

#Analyze rider length data by customer type and weekday
View(clean_trip_final %>% 
       group_by(costumer_type, week_day) %>% 
       summarise(number_of_ride = n(),
                 avgerage_duration = mean(ride_length),
                 median_duration = median(ride_length),
                 max_duration = max(ride_length),
                 min_duration = min(ride_length)))

#Analyze rider length data by customer type and month
View(clean_trip_final %>% 
       group_by(costumer_type, month) %>% 
       summarise(nummber_of_ride = n(),
                 average_duration = mean(ride_length),
                 median_duration = median(ride_length),
                 max_duration = max(ride_length),
                 min_duration = min(ride_length)))

#Save the data for data visualization
write.csv(clean_trip_final,file = "clean_trip_final_tableau.csv",row.names = FALSE)
