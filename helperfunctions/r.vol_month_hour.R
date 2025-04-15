# This function loops the r.vol function in order to compute a vector of 
# a month's daily realised volatility, for each hour of each day

#Necessary libraries
library(dplyr)

#Necessary function
#source(here("helperfunctions/r.vol_hour.R"))

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
    r_vol_h[,j] = t(r.vol_hour(daydata)) #computes realized volatility for each day
  }
  return(r_vol_h)
}

#e.g. r.vol_month_hour(raw_SPY2021_01)