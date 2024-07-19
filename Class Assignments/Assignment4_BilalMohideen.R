#Assignment #4 - Martians
#Bilal Mohideen - BTC1855H
#R Version 2024.04.1+748 (2024.04.1+748)

#ensure that "ufo_subset.csv" is saved in the working directory
getwd()

#loading the required packages for cleaning the data set
library(tidyverse)
library(lubridate)
library(magrittr)

#importing the raw data as "ufo_data"
ufo_data <- read.csv("ufo_subset.csv", header = TRUE)

#creating a copy of the data set for data manipulation and analysis
ufo_data1 <- ufo_data

#verifying that the data set has been imported as a data frame
class(ufo_data1)

#verifying consistency in column names
names(ufo_data1)

#viewing the structure of the data frame
str(ufo_data1)

#viewing the data set
view(ufo_data1)
head(ufo_data1)
tail(ufo_data1)

#summarizing the data set
summary(ufo_data1)

#filtering out duplicate entries
ufo_data1 <- ufo_data1 %>% distinct()

#checking for NA values in the key columns of interest
#(country, shape, and duration.seconds)
any(is.na(ufo_data1$country))
any(is.na(ufo_data1$shape))
any(is.na(ufo_data1$duration.seconds))

#no NA values detected in any of the columns
#taking another look at each of the columns
# country column:
view(ufo_data1$country)
any(ufo_data1$country == "")

#shape column:
view(ufo_data1$shape)
any(ufo_data1$shape == "")

#duration.seconds column:
view(ufo_data1$duration.seconds)
any(ufo_data1$duration.seconds == "")

#missing values are not NA, just recorded as blank values
#for country and shape columns
#duration.seconds does not have any blank values
sum(ufo_data1$country == "")
sum(ufo_data1$shape == "")
sum(ufo_data1$duration.seconds == "")

#verifying that duration.seconds is a numeric class
class(ufo_data1$duration.seconds)

#checking for unreasonable values for duration in seconds: 
#less than 1 second (negative values and zero should not be included)
#i have also decided that sightings less than 1 second are suspicious
#sightings that are exactly 1 second are OK
#sightings are split by date, so sightings longer than 1 day
#should be considered unreasonable as well
#1 day = 86400 seconds
any(ufo_data1$duration.seconds < 1)
any(ufo_data1$duration.seconds >= 86400)

#excluding missing values and outliers for key columns of interest
#(country, shape, and duration.seconds)
ufo_data1 <- ufo_data1 %>% filter(ufo_data1$country != "")
ufo_data1 <- ufo_data1 %>% filter(ufo_data1$shape != "")
ufo_data1 <- ufo_data1 %>% filter(ufo_data1$duration.seconds > 1)
ufo_data1 <- ufo_data1 %>% filter(ufo_data1$duration.seconds < 86400)

#to identify and remove hoax sightings:
#screen for rows with "NUFORC Note:" appearing in comments
ufo_data1 <- ufo_data1 %>% 
  filter(!str_detect(ufo_data1$comments, pattern = "NUFORC Note:", 
                     negate = FALSE))

#to add a report_delay column
#convert sighting's time to year-month-date-hour-minute format
ufo_data1$datetime <- ymd_hm(ufo_data1$datetime)

#convert date posted to day-month-year format
ufo_data1$date_posted <- dmy(ufo_data1$date_posted)

#confirm that sighting's time is in POSIXct structure
str(ufo_data1$datetime)

#convert date posted to POSIXct structure
ufo_data1$date_posted <- as.POSIXct(ufo_data1$date_posted)

#confirm that date posted is in POSIXct structure
str(ufo_data1$date_posted)

#calculate the time difference in days between date of sighting
#and date it was reported
report_delay <- difftime(ufo_data1$date_posted, ufo_data1$datetime, 
                            units = c("days"))

#adding new column to the data set
ufo_data1 <- ufo_data1 %>% mutate(report_delay)

#removing rows when sighting was reported before happening
#anything with report delay less than 0 (negative) is filtered out
ufo_data1 <- ufo_data1 %>% filter(report_delay >= 0)

#creating table with average report_delay for each country
mean_report_delay <- ufo_data1 %>% 
  group_by(ufo_data1$country) %>%
  summarize(mean_report_delay = mean(report_delay))

#viewing the table for average report delay by country
view(mean_report_delay)

#creating histogram using the duration.seconds column
#use log for better visualization
hist(log(ufo_data1$duration.seconds), main = "Frequency of UFO Sightings",
     xlab = "log Duration (seconds)", ylab = "Frequency of sightings")


