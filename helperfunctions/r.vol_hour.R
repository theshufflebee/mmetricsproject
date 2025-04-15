# This function computes the realised volatily of each hour in one day
# it outputs a vector 

#Necessary libraries
library(dplyr)

#--------------------------------------------------------------------------------

#t data is data for one day, great place for the day_selector function!
r.vol_hour = function(t_data){
  hours = unique(format(as.POSIXct(t_data$timestamp, 
                                   format = "%Y-%m-%d %H:%M:%S"),format="%H"))
  day = unique(format(as.Date(t_data$timestamp, 
                              format = "%Y-%m-%d %H:%M:%S"),format="%d"))
  month = unique(format(as.Date(t_data$timestamp, 
                                format = "%Y-%m-%d %H:%M:%S"),format="%m"))
  year = unique(format(as.Date(t_data$timestamp, 
                               format = "%Y-%m-%d %H:%M:%S"),format="%Y"))
  r_vol = c(1:length(hours)) #preparing solution vector
  for (i in 1:length(hours)){
    date = paste(year,month,day,sep="-")
    date = paste(date,hours[i])
    newdata = filter(t_data, str_detect(timestamp, date))
    p_h = newdata$close
    p_h_1 <- lag(p_h, 1)
    delta_price = p_h_1 - p_h
    delta_price_sqr = delta_price^2
    v_h = sum(na.omit(delta_price_sqr)) / length(na.omit(delta_price)) #avg per hour
    r_vol[i] = v_h
  }
  return(r_vol)
}

#example: r.vol_hour(day_SPY_0402)