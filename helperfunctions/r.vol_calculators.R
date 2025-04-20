# This file contains all functions related to calculating realised volatility

#Necessary libraries
library(dplyr)

#Necessary functions
#source(here("helperfunctions/date_selector.R"))

#--------------------------------------------------------------------------------
#-----------------                     1                        -----------------
#--------------------------------------------------------------------------------

# This function computes a day's realised volatility

#--------------------------------------------------------------------------------

#data must be a single day's data

r.vol_day = function(data){
  p_t = data$close
  #delta_price = diff(p_t)
  delta_price = diff(log(p_t))
  delta_price_sqr = delta_price^2
  v_t = mean(delta_price_sqr, na.rm=T)
  return(v_t) #t as the day
}

#example: r.vol_day(day_SPY_0402)
#outputs the realised volatility for the 2nd of April



#--------------------------------------------------------------------------------
#-----------------                     2                        -----------------
#--------------------------------------------------------------------------------

# This function computes the realised volatility of each hour in one day
# it outputs a vector of length = nbr of hours

#--------------------------------------------------------------------------------

#data must be data for one day, great place for the day_selector function!


r.vol_day_hour <- function(data) {
  data$timestamp <- as.POSIXct(data$timestamp)
  data$hour <- format(data$timestamp, "%H")
  hours <- unique(data$hour)
  r_vol <- numeric(length(hours))
  
  for (h in seq_along(hours)) {
    subset_data <- data[data$hour == hours[h], ]
    p_h <- subset_data$close
    #delta_price <- diff(p_h)
    delta_price <- diff(log(p_h))
    delta_price_sqr = delta_price^2
    r_vol[h] <- mean(delta_price_sqr, na.rm = TRUE)
  }
  
  return(r_vol)
}


#example: r.vol_day_hour(day_SPY_0402)



#--------------------------------------------------------------------------------
#-----------------                     3                        -----------------
#--------------------------------------------------------------------------------

# This function loops the r.vol function in order to compute a vector of 
# a whole month's daily realised volatility

#--------------------------------------------------------------------------------

#data as a csv file containing a month of data


r.vol_month <- function(data) {
  data$timestamp <- as.POSIXct(data$timestamp)
  data$date <- as.Date(data$timestamp)
  days <- unique(data$date)
  r_vol <- numeric(length(days))
  
  for (d in seq_along(days)) {
    daydata <- data[data$date == days[d], ]
    r_vol[d] <- r.vol_day(daydata)
  }
  return(r_vol)
}


#e.g. r.vol_month(raw_SPY2021_01)


#--------------------------------------------------------------------------------
#-----------------                     4                        -----------------
#--------------------------------------------------------------------------------

# This function loops the r.vol function in order to compute a matrix of 
# a month's daily realised volatility, for each hour of each day

#--------------------------------------------------------------------------------

#data as a csv file containing only 1 month of data


r.vol_month_hour <- function(data) {
  data$timestamp <- as.POSIXct(data$timestamp)
  data$date <- as.Date(data$timestamp)
  days <- unique(data$date)
  
  #take the first day to count how many hours the market is open
  first_day <- data[data$date == days[1], ]
  hours <- unique(format(first_day$timestamp, "%H"))
  r_vol_h <- matrix(NA, nrow = length(hours), ncol = length(days))
  
  for (d in seq_along(days)) {
    daydata <- data[data$date == days[d], ]
    r_vol_h[, d] <- r.vol_day_hour(daydata)
  }
  return(r_vol_h)
}


#e.g. r.vol_month_hour(raw_SPY2021_01)


#--------------------------------------------------------------------------------
#-----------------                     5                        -----------------
#--------------------------------------------------------------------------------

# This function loops the r.vol function in order to compute a matrix of 
# a whole year's daily realised volatility

#--------------------------------------------------------------------------------

#data as a csv file with full data


r.vol_daily <- function(data, merge = FALSE) {
  original_colnames <- colnames(data)
  data$timestamp <- as.POSIXct(data$timestamp)
  data$date <- as.Date(data$timestamp)
  days <- unique(data$date)
  
  r_vol_list <- list()
  
  for (d in seq_along(days)) { #loop for each day
    
    #select data for that day
    daydata <- data[data$date == days[d], ]
    
    #calculate volatility
    day_timestamp = as.POSIXct(format(days[d], "%Y-%m-%d"))
    vol <- r.vol_day(daydata)
    
    #add to list
    r_vol_day <- data.frame(timestamp = day_timestamp, r_vol_d = vol)
    r_vol_list[[d]] <- r_vol_day
  }
  r_vol_y <- do.call(rbind, r_vol_list)
  
  #option to merge calculated volatility with original data
  if (merge == TRUE) {
    
    #format for days 
    data$timestamp_day <- as.POSIXct(format(data$timestamp, "%Y-%m-%d"))
    
    #merge by matching the days
    merged_data <- merge(data, r_vol_y, by.x = "timestamp_day", by.y = "timestamp", all.x = TRUE)
    
    #make sure its ordered by minute (merging by day messes it up)
    merged_data <- merged_data[order(merged_data$timestamp), ]
    
    #get original column order
    final_data <- merged_data[, c(original_colnames, "r_vol_d")]
    return(final_data)
  } else {
    colnames(r_vol_y)[1] <- "timestamp"
    return(r_vol_y)
  }
}



#e.g. r.vol_year(raw_SPY_2024)


#--------------------------------------------------------------------------------
#-----------------                     6                        -----------------
#--------------------------------------------------------------------------------

# This function loops the r.vol function in order to compute a matrix of 
# a whole year's hourly realised volatility
#works for datasets with more than 1 year!

#--------------------------------------------------------------------------------

#data as a csv file can be for a year but also more


r.vol_hourly <- function(data, merge = FALSE) {
  original_colnames <- colnames(data)
  data$timestamp <- as.POSIXct(data$timestamp)
  data$date <- as.Date(data$timestamp)
  days <- unique(data$date)
  
  r_vol_list <- list()
  
  for (d in seq_along(days)) { #loop for each day
    
    #select data for that day
    daydata <- data[data$date == days[d], ]
    
    #isolate hours and calculate volatility
    hours <- unique(format(daydata$timestamp, "%Y-%m-%d %H:00:00"))
    vol <- r.vol_day_hour(daydata)
    
    #add to list
    r_vol_day <- data.frame(timestamp = as.POSIXct(hours), r_vol_h = vol)
    r_vol_list[[d]] <- r_vol_day
  }
  r_vol_y <- do.call(rbind, r_vol_list)
  
  #option to merge calculated volatility with original data
  if (merge == TRUE) {
    
    #format for hours 
    data$timestamp_hour <- as.POSIXct(format(data$timestamp, "%Y-%m-%d %H:00:00"))
    
    #merge by matching the hours
    merged_data <- merge(data, r_vol_y, by.x = "timestamp_hour", by.y = "timestamp", all.x = TRUE)
    
    #make sure its ordered by minute (merging by hour messes it up)
    merged_data <- merged_data[order(merged_data$timestamp), ]
    
    #get original column order
    final_data <- merged_data[, c(original_colnames, "r_vol_h")]
    return(final_data)
  } else {
    colnames(r_vol_y)[1] <- "timestamp"
    return(r_vol_y)
  }
}


#e.g. r.vol_year_hour(raw_SPY_2024) #default merge = False
#e.g. with merge: r.vol_year_hour(raw_SPY_2024,merge=T)











