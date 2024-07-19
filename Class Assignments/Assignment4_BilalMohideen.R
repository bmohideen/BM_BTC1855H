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

#verify that missing values and outliers have been removed from each column
view(ufo_data1$country)
view(ufo_data1$shape)
view(ufo_data1$duration.seconds)






