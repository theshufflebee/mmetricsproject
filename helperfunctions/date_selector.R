# This file contains all functions related to selecting subsets of data

#Necessary libraries
library(dplyr)
library(zoo)

#--------------------------------------------------------------------------------
#-----------------                     1                        -----------------
#--------------------------------------------------------------------------------

# This function selects a day from financial market data
# Will filter out all datapoints for that day


#--------------------------------------------------------------------------------

#must input rawdata (csv file)
#year, month and day input as numbers, no need to write "02" instead of "2"


day_selector <- function(rawdata, year, month, day) {
  rawdata$timestamp <- as.POSIXct(rawdata$timestamp)
  day_to_filter <- as.Date(sprintf("%04d-%02d-%02d", year, month, day))
  newdata <- rawdata[as.Date(rawdata$timestamp) == day_to_filter, ]
  return(newdata) #as.Date faster for full dates, format better for partial
}


#example: day_ONEQ_0409 = day_selector(raw_ONEQ,2025,04,09) 
#selects data for the 9th of April 2025


#--------------------------------------------------------------------------------
#-----------------                     2                        -----------------
#--------------------------------------------------------------------------------

# This function selects a month from financial market data
# Will filter out all datapoints for that month

#--------------------------------------------------------------------------------

#must input rawdata (csv file)
#year, month input as numbers, no need to write "02" instead of "2"


month_selector <- function(rawdata, year, month) {
  rawdata$timestamp <- as.POSIXct(rawdata$timestamp)
  month_to_filter <- sprintf("%04d-%02d", year, month)
  newdata <- rawdata[format(rawdata$timestamp, "%Y-%m") == month_to_filter, ]
  return(newdata)
}


#example: month_ONEQ_04 = month_selector(raw_ONEQ_2025,2025,04) 
#selects data for April 2025


#--------------------------------------------------------------------------------
#-----------------                     3                        -----------------
#--------------------------------------------------------------------------------

# This function selects a year from financial market data
# Will filter out all datapoints for that year

#--------------------------------------------------------------------------------

#must input rawdata (csv file)
#year input as number


year_selector <- function(rawdata, year) {
  rawdata$timestamp <- as.POSIXct(rawdata$timestamp)
  year_to_filter <- as.character(year)
  newdata <- rawdata[format(rawdata$timestamp, "%Y") == year_to_filter, ]
  return(newdata)
}


#example: ONEQ_24 = month_selector(ONEQ,2024) 
#selects data for 2024 out of a full dataset




