# This function loops the r.vol function in order to compute a vector of 
# a whole year's daily realised volatility

#Necessary libraries
library(dplyr)

#Necessary function
#source(here("helperfunctions/r.vol.R"))
#source(here("helperfunctions/r.vol_month.R"))
#source(here("helperfunctions/date_selector.R"))

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
    r_vol_m[,m] = replace(r_vol_m[,m],mdays,t(r.vol_month(monthdata))) #computes realized volatility for each month
  }
  return(r_vol_m)
}

#e.g. r.vol_year(raw_SPY_2024)