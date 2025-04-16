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
  p_t_1 <- lag(p_t, 1)
  delta_price = p_t_1 - p_t
  delta_price_sqr = delta_price^2
  v_t = sum(na.omit(delta_price_sqr)) / length(na.omit(delta_price))
  return(v_t) #t as the day
}

#example: r.vol(day_SPY_0402)
#outputs the realised volatility for the 2nd of April



#--------------------------------------------------------------------------------
#-----------------                     2                        -----------------
#--------------------------------------------------------------------------------

# This function computes the realised volatily of each hour in one day
# it outputs a vector of length = nbr of hours

#--------------------------------------------------------------------------------

#data must be data for one day, great place for the day_selector function!
r.vol_day_hour = function(data){
  hours = unique(format(as.POSIXct(data$timestamp, 
                                   format = "%Y-%m-%d %H:%M:%S"),format="%H"))
  day = unique(format(as.Date(data$timestamp, 
                              format = "%Y-%m-%d %H:%M:%S"),format="%d"))
  month = unique(format(as.Date(data$timestamp, 
                                format = "%Y-%m-%d %H:%M:%S"),format="%m"))
  year = unique(format(as.Date(data$timestamp, 
                               format = "%Y-%m-%d %H:%M:%S"),format="%Y"))
  r_vol = c(1:length(hours)) #preparing solution vector
  for (i in 1:length(hours)){
    date = paste(year,month,day,sep="-")
    date = paste(date,hours[i])
    newdata = filter(data, str_detect(timestamp, date))
    p_h = newdata$close
    p_h_1 <- lag(p_h, 1)
    delta_price = p_h_1 - p_h
    delta_price_sqr = delta_price^2
    v_h = sum(na.omit(delta_price_sqr)) / length(na.omit(delta_price)) #avg per hour
    r_vol[i] = v_h #v_h would be volatility for each hour, which is put in a vector
  }
  return(r_vol)
}

#example: r.vol_hour(day_SPY_0402)




















#--------------------------------------------------------------------------------
#-----------------                     3                        -----------------
#--------------------------------------------------------------------------------



















#--------------------------------------------------------------------------------
#-----------------                     4                        -----------------
#--------------------------------------------------------------------------------












#--------------------------------------------------------------------------------
#-----------------                     5                        -----------------
#--------------------------------------------------------------------------------








