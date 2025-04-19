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

#example: r.vol_day(day_SPY_0402)
#outputs the realised volatility for the 2nd of April



#--------------------------------------------------------------------------------
#-----------------                     2                        -----------------
#--------------------------------------------------------------------------------

# This function computes the realised volatility of each hour in one day
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

#example: r.vol_day_hour(day_SPY_0402)



#--------------------------------------------------------------------------------
#-----------------                     3                        -----------------
#--------------------------------------------------------------------------------

# This function loops the r.vol function in order to compute a vector of 
# a whole month's daily realised volatility

#--------------------------------------------------------------------------------

#data as a csv file containing a month of data
r.vol_month = function(data){
  days = unique(format(as.Date(data$timestamp, #find all days excluding weekends etc
                               format = "%Y-%m-%d %H:%M:%S"),format="%d"))
  month = unique(format(as.Date(data$timestamp, #for day selector
                                format = "%Y-%m-%d %H:%M:%S"),format="%m"))
  month = as.numeric(month) #due to how day selector input is defined
  year = unique(format(as.Date(data$timestamp, #for day selector
                               format = "%Y-%m-%d %H:%M:%S"),format="%Y"))
  year = as.numeric(year)
  r_vol = c(1:length(days)) #preparing solution vector
  
  for (i in 1:length(days)){ #loop for each day
    daydata = day_selector(data,year,month,as.numeric(days[i])) #selects data for each day
    r_vol[i] = r.vol_day(daydata) #computes realized volatility for each day
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
r.vol_month_hour = function(data){
  
  #prepare dates
  ndays = unique(format(as.Date(data$timestamp, #find all days excluding weekends etc
                                format = "%Y-%m-%d %H:%M:%S"),format="%d"))
  nmonth = unique(format(as.Date(data$timestamp, #for day selector
                                 format = "%Y-%m-%d %H:%M:%S"),format="%m"))
  nmonth = as.numeric(nmonth) #due to how day selector input is defined
  nyear = unique(format(as.Date(data$timestamp, #for day selector
                                format = "%Y-%m-%d %H:%M:%S"),format="%Y"))
  nyear = as.numeric(nyear)
  
  #take the first day to count how many hours the market is open
  ndaydata = day_selector(data,nyear,nmonth,as.numeric(ndays[1]))
  nhours = unique(format(as.POSIXct(ndaydata$timestamp, 
                                    format = "%Y-%m-%d %H:%M:%S"),format="%H"))
  
  #prepare solution matrix, columns are days, rows are hours
  r_vol_h = matrix(c(1:length(days)),length(nhours),length(ndays))
  
  for (j in 1:length(ndays)){ #loop for each day
    daydata = day_selector(data,nyear,nmonth,as.numeric(ndays[j])) #selects data for each day
    r_vol_h[,j] = t(r.vol_day_hour(daydata)) #computes realized volatility for each day
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

#data as a csv file containing a year of data
r.vol_year = function(data){
  mmonths = unique(format(as.Date(data$timestamp, #find all months
                                  format = "%Y-%m-%d %H:%M:%S"),format="%m"))
  myear = unique(format(as.Date(data$timestamp, #for day selector
                                format = "%Y-%m-%d %H:%M:%S"),format="%Y"))
  myear = as.numeric(myear)
  
  #columns are months, rows are days
  r_vol_m = matrix(NA,31,length(mmonths)) #preparing solution matrix
  
  for (m in 1:length(mmonths)){ #loop for each month
    monthdata = month_selector(data,myear,as.numeric(mmonths[m])) #selects data for each month
    mdays =  unique(format(as.Date(monthdata$timestamp, 
                                   format = "%Y-%m-%d %H:%M:%S"),format="%d"))
    mdays = as.numeric(mdays)
    month_vol = t(r.vol_month(monthdata)) #computes realized volatility for each month
    r_vol_m[,m] = replace(r_vol_m[,m],mdays,month_vol) 
  }
  return(r_vol_m)
}

#e.g. r.vol_year(raw_SPY_2024)




#--------------------------------------------------------------------------------
#-----------------                     6                        -----------------
#--------------------------------------------------------------------------------

# This function loops the r.vol function in order to compute a matrix of 
# a whole year's hourly realised volatility
#works for datasets with more than 1 year!

#--------------------------------------------------------------------------------

#data as a csv file containing a year of data
r.vol_year_hour = function(data,merge=F){
  
  original_colnames <- colnames(data)
  
  days = unique(format(as.Date(data$timestamp, 
                               format = "%Y-%m-%d %H:%M:%S"),format="%Y-%m-%d"))
  
  days = as.Date(days,format = "%Y-%m-%d")
  
  r_vol_y = setNames(data.frame(matrix(ncol = 2, nrow = 0)), 
                     c("timestamp", "r_vol_h"))
  r_vol_list <- list()
  
  for (d in 1:length(days)){ #loop for each day
    
    #find day date
    dyear = as.numeric(format(days[d],format="%Y"))
    dmonth = as.numeric(format(days[d],format="%m"))
    ddays = as.numeric(format(days[d],format="%d"))
    
    #select data for that day
    daydata = day_selector(data,dyear,dmonth,ddays) 
    
    #isolate hours and calculate volatility
    dhours = unique(trunc(daydata$timestamp, units = "hours"))
    r_vol_h = (r.vol_day_hour(daydata)) #computes realized volatility for each day
    
    #add to list
    r_vol_d = data.frame(timestamp_hour = dhours, r_vol_h = r_vol_h)
    r_vol_list[[d]] = r_vol_d 
  }
  r_vol_y <- do.call(rbind, r_vol_list)
  
  if (merge == T) {
    #option to merge calculated volatility with original data
    data$timestamp_hour = trunc(data$timestamp, units = "hours")
    data$timestamp_hour = as.POSIXct(data$timestamp_hour, 
                                     format="%Y-%m-%d %H:%M:%S")
    
    #merge with the original dataset 
    data <- merge(data, r_vol_y, by = "timestamp_hour", all.x = TRUE)
    data <- data[order(data$timestamp), ]
    final_col_order <- c(original_colnames, "r_vol_h")
    data <- data[, final_col_order]
    return(data)}
  else {names(r_vol_y)[1]<-paste("timestamp")
        return(r_vol_y)}
}

#e.g. r.vol_year_hour(raw_SPY_2024) #default merge = False
#e.g. with merge: r.vol_year_hour(raw_SPY_2024,merge=T)









