# This function loops the r.vol function in order to compute a vector of 
# a month's daily realised volatility

#Necessary libraries
library(dplyr)

#Necessary function
#source(here("helperfunctions/r.vol.R"))

#--------------------------------------------------------------------------------

#data as a csv file containing only 1 month of data
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
    r_vol[i] = r.vol(daydata) #computes realized volatility for each day
  }
  return(r_vol)
}

#e.g. r.vol_month(raw_SPY2021_01)