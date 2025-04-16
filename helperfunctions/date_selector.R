# This file contains all functions related to selecting subsets of data

#Necessary libraries
library(dplyr)

#--------------------------------------------------------------------------------
#-----------------                     1                        -----------------
#--------------------------------------------------------------------------------

# This function selects a day from financial market data
# Will filter out all datapoints for that day


#--------------------------------------------------------------------------------

#must input rawdata (csv file)
#year, month and day input as numbers, no need to write "02" instead of "2"

day_selector <- function(rawdata,year,month,day){
  date = paste("^",year,"-",sprintf("%02d",month),"-",sprintf("%02d",day),sep="")
  newdata = filter(rawdata, str_detect(timestamp, date))
  newdata$timestamp = as.POSIXct(newdata$timestamp, 
                                 format = "%Y-%m-%d %H:%M:%S")
  return(newdata)
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

month_selector <- function(rawdata,year,month){
  date = paste("^",year,"-",sprintf("%02d",month),sep="")
  newdata = filter(rawdata, str_detect(timestamp, date))
  newdata$timestamp = as.POSIXct(newdata$timestamp, 
                                 format = "%Y-%m-%d %H:%M:%S")
  return(newdata)
}

#example: month_ONEQ_04 = month_selector(raw_ONEQ_2025,2025,04) 
#selects data for April 2025