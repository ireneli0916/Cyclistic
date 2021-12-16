# Google Data Analysis Certifcate - Capstone Project
# Analysis Cyclist bike using 
# and determine the way to convert casual users to annual membership
# Irene Li
# date created: 2021-12-06 03:38PM

### install require packages
#### tidyverse for data import and wrangling 
install.packages("tidyverse")
install.packages("dplyr")
#### lubridate for date functions, works with dates and time
install.packages("lubridate")
#### ggplot for data visualization, apply different visual properties 
install.packages("ggplot2")


### call the downloaded packages
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)


### set my working directory
#### all data have been uploaded to a folder called "CSV"
getwd()
setwd("/Users/irene/Desktop/Google_Project/CSV_files")


# STEP 1: COLLECT DATA (UPLOAD DATA) for the previous 12 months
trip_q1_2020 <- read.csv("2020_Q1_trips.csv")
trip_04_2020 <- read.csv("2020_04_trips.csv")
trip_05_2020 <- read.csv("2020_05_trips.csv")
trip_06_2020 <- read.csv("2020_06_trips.csv")
trip_07_2020 <- read.csv("2020_07_trips.csv")
trip_08_2020 <- read.csv("2020_08_trips.csv")
trip_09_2020 <- read.csv("2020_09_trips.csv")
trip_10_2020 <- read.csv("2020_10_trips.csv")
trip_11_2020 <- read.csv("2020_11_trips.csv")
trip_12_2020 <- read.csv("2020_12_trips.csv")


# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
### check the columns names of each file to make sure they are consistent, so we can merge them together
colnames(trip_q1_2020)
colnames(trip_04_2020)
colnames(trip_05_2020)
colnames(trip_06_2020)
colnames(trip_07_2020)
colnames(trip_08_2020)
colnames(trip_09_2020)
colnames(trip_10_2020)
colnames(trip_11_2020)
colnames(trip_12_2020)


### check data frames and look for any incongruities
str(trip_q1_2020)
str(trip_04_2020)
str(trip_05_2020)
str(trip_06_2020)
str(trip_07_2020)
str(trip_08_2020)
str(trip_09_2020)
str(trip_10_2020)
str(trip_11_2020)
str(trip_12_2020)


### we need to convert type of start_station_id and end_station_id from int to chr
### so data frames can merge correctly and keep its consistency
trip_q1_2020 <- mutate(trip_q1_2020, start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
trip_04_2020 <- mutate(trip_04_2020, start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
trip_05_2020 <- mutate(trip_05_2020, start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
trip_06_2020 <- mutate(trip_06_2020, start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
trip_07_2020 <- mutate(trip_07_2020, start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
trip_08_2020 <- mutate(trip_08_2020, start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
trip_09_2020 <- mutate(trip_09_2020, start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
trip_10_2020 <- mutate(trip_10_2020, start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
trip_11_2020 <- mutate(trip_11_2020, start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
trip_12_2020 <- mutate(trip_12_2020, start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))


### all variables have been checked for consistency, and ready to merge into one big data frame
all_trips <- bind_rows(trip_q1_2020, trip_04_2020, trip_05_2020, trip_06_2020, trip_07_2020, trip_08_2020,
                   trip_09_2020, trip_10_2020, trip_11_2020, trip_12_2020)


### remove variables that will not be used in the analysis 
all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng, start_station_name, start_station_id, end_station_name, end_station_id))


# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
### inspect the new table that has been created
colnames(all_trips)
nrow(all_trips)
dim(all_trips)
head(all_trips)
str(all_trips)
summary(all_trips)


### check categories under different columns
ride_cat <- unique(all_trips$rideable_type)
ride_cat
user_cat <- unique(all_trips$member_casual)
user_cat


### check numbers of each user type
### we have 1366575 for casual and 2175108 for member 
table(all_trips$member_casual)


### check numbers of each bike type
### we have 70616 for classic_bike, 2966322 for docked_bike, 504745 for electric_bike
table(all_trips$rideable_type)


### add columns of date, month, day, and year of each ride, also the day of the week
all_trips$date <- as.Date(all_trips$started_at) #extract date information from the column
all_trips$day <- format(as.Date(all_trips$started_at), "%d") # format the date information for day
all_trips$month <- format(as.Date(all_trips$started_at), "%m") #format the date information for month
all_trips$year <- format(as.Date(all_trips$started_at), "%Y") #format the date information for year in 4-digit
all_trips$day_of_week <- format(as.Date(all_trips$started_at), "%A") #format the date information for day of the week using unabbreviated weekday


### add column ride_length to data frame in minutes
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at, units = "mins")


### inspect the structure of the columns
str(all_trips)


### convert ride_length from double to numeric so we can perform calculation
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)


# REMOVE "BAD" DATA
### remove entries that ride_length was negative
all_trips_2 <- all_trips[!(all_trips$ride_length<=0),]
sum(all_trips$ride_length<=0)
sum(all_trips_2$ride_length<=0)


### remove entries that ride_length was more than one day (1440 mins)
sum(all_trips$ride_length>=1440)
all_trips_2 <- all_trips_2[!(all_trips_2$ride_length>=1440),]
sum(all_trips_2$ride_length>=1440)


# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
### descriptive analysis on ride_length
mean(all_trips_2$ride_length) # 23.5301 mins
median(all_trips_2$ride_length) # 14.1 mins
min(all_trips_2$ride_length) # 0.0167 mins
max(all_trips_2$ride_length) # 1439.817 mins
summary(all_trips_2$ride_length) # units in mins


### compare members and casual users on ride_length
aggregate(all_trips_2$ride_length ~ all_trips_2$member_casual, FUN = mean)
aggregate(all_trips_2$ride_length ~ all_trips_2$member_casual, FUN = median)
aggregate(all_trips_2$ride_length ~ all_trips_2$member_casual, FUN = min)
aggregate(all_trips_2$ride_length ~ all_trips_2$member_casual, FUN = max)


### compare average ride time by each day for members and casual users
aggregate(all_trips_2$ride_length ~ all_trips_2$member_casual + all_trips_2$day_of_week, FUN = mean)


### days of the week are out of order, fix this problem 
all_trips_2$day_of_week <- ordered(all_trips_2$day_of_week, levels = c("Sunday","Monday",
                                                                       "Tuesday","Wednesday",
                                                                       "Thursday","Friday",
                                                                       "Saturday"))


### compute again the average ride time by each day for members and casuals
aggregate(all_trips_2$ride_length ~ all_trips_2$member_casual + all_trips_2$day_of_week, FUN = mean)



### analyze ridership data by type and weekday
all_trips_2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday)


### visualize the number of rides based on rider type
all_trips_2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  ggtitle("Number of Bikes based on Rider Type")


### visualize average duration
all_trips_2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  ggtitle("Average Duration based on Rider Type")


# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
counts <- aggregate(all_trips_2$ride_length ~ all_trips_2$member_casual + 
                      all_trips_2$day_of_week, FUN = mean)
### export and save the csv file in the work direction
write.csv(counts, file = "/Users/irene/Desktop/Google_Project/CSV_files/avg_ride_length.csv")














