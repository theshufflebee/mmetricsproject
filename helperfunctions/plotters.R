# This function plots intraday financial data at 1-min intervals

#Necessary libraries
library(ggplot2)
library(lubridate)


#--------------------------------------------------------------------------------
#-----------------                     1                        -----------------
#--------------------------------------------------------------------------------

# This function plots daily close prices per minute for a day

#--------------------------------------------------------------------------------

#input a day's financial data 
#input title as string

price_plotter_day = function(data,title){
  ggplot(data, aes(x = timestamp, y = close)) +
    geom_point(color = "blue", size = 0.01) +
    geom_line(aes(group=1)) +
    labs(title = title,
         x = "Time",
         y = "Close Price") +
    scale_x_datetime(date_labels = "%H:%M",  
                     date_breaks = "60 min") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#example: price_plotter_day(day_ONEQ_0409,"ONEQ Price on April 9th")




#--------------------------------------------------------------------------------
#-----------------                     2                        -----------------
#--------------------------------------------------------------------------------

# This function plots daily close prices per minute for a month

#--------------------------------------------------------------------------------

#input a month's financial data 
#input title as string

price_plotter_month = function(data,title){
  ggplot(data, aes(x = as.POSIXct(timestamp), y = close)) +
    geom_point(color = "blue", size = 0.01) +
    geom_line(aes(group=1), linewidth=0.05) +
    labs(title = title,
         x = "Time",
         y = "Close Price") +
    scale_x_datetime(date_labels = "%d", date_breaks = "2 day") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#example: price_plotter_month(ONEQ_04,"ONEQ Price in April")



#--------------------------------------------------------------------------------
#-----------------                     3                        -----------------
#--------------------------------------------------------------------------------

# This function plots daily close prices per minute for a year

#--------------------------------------------------------------------------------

#input a year's financial data 
#input title as string

price_plotter_year = function(data,title){
  ggplot(data, aes(x = as.POSIXct(timestamp), y = close)) +
    geom_point(color = "blue", size = 0.01) +
    geom_line(aes(group=1), linewidth=0.05) +
    labs(title = title,
         x = "Time",
         y = "Close Price") +
    scale_x_datetime(date_labels = "%b", date_breaks = "1 month") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#example: price_plotter_year(ONEQ_2024,"ONEQ Price 2024")




#--------------------------------------------------------------------------------
#-----------------                     4                        -----------------
#--------------------------------------------------------------------------------

# This function plots daily close prices per minute for over a year data

#--------------------------------------------------------------------------------

#input big financial data 
#input title as string

price_plotter = function(data,title){
  ggplot(data, aes(x = as.POSIXct(timestamp), y = close)) +
    geom_point(color = "blue", size = 0.01) +
    geom_line(aes(group=1), linewidth=0.05) +
    labs(title = title,
         x = "Time",
         y = "Close Price") +
    scale_x_datetime(date_labels = "%b %Y", date_breaks = "6 month") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(face = "bold", hjust = 0.5))
}

#example: price_plotter(raw_ONEQ,"ONEQ Price Over Time")







#--------------------------------------------------------------------------------
#-----------------                     5                        -----------------
#--------------------------------------------------------------------------------

# This function plots daily realised volatility data

#--------------------------------------------------------------------------------

#for daily vol
#breaks and title as strings
#breaks can be monthly, daily, or hourly

dvol_plotter = function(data,breaks,title){

  x_scale <- switch(breaks,
                    "yearly" = scale_x_datetime(date_labels = "%b %Y", date_breaks = "6 month"),
                    "monthly" = scale_x_datetime(date_labels = "%b %Y", date_breaks = "1 month"),
                    "daily"   = scale_x_datetime(date_labels = "%a %d", date_breaks = "1 day"),
                    "hourly"  = scale_x_datetime(date_labels = "%Hh", date_breaks = "1 hour"),
                    NULL)  #default NULL if nothing matches   
  
  ggplot(data, aes(x = timestamp, y = r_vol_d)) +
    geom_line(color = "#2c7fb8", linewidth = 0.9) +
    geom_point(color = "#253494", size = 1) +
    x_scale + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    labs(title = title,
         x = NULL,
         y = "daily realised volatility") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(face = "bold", hjust = 0.5))
}

#e.g. vol_plotter(vol_SPY2024,breaks="1 month",
#                              title="Realised Volatility - SPY 2024")




#--------------------------------------------------------------------------------
#-----------------                     6                        -----------------
#--------------------------------------------------------------------------------

# This function plots hourly realised volatility data

#--------------------------------------------------------------------------------

#for hourly vol
#breaks and title as strings 
#breaks can be monthly, daily, or hourly

hvol_plotter = function(data,breaks,title){
  
  x_scale <- switch(breaks,
                    "yearly" = scale_x_datetime(date_labels = "%b %Y", date_breaks = "6 month"),
                    "monthly" = scale_x_datetime(date_labels = "%b %Y", date_breaks = "1 month"),
                    "daily"   = scale_x_datetime(date_labels = "%a %d", date_breaks = "1 day"),
                    "hourly"  = scale_x_datetime(date_labels = "%Hh", date_breaks = "1 hour"),
                    NULL)  #default NULL if nothing matches   
  
  ggplot(data, aes(x = timestamp, y = r_vol_h)) +
    geom_line(color = "#2c7fb8", linewidth = 0.9) +
    geom_point(color = "#253494", size = 1.3) +
    x_scale + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    labs(title = title,
         x = NULL,
         y = "hourly realised volatility") +
    theme_minimal(base_size = 14) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(face = "bold", hjust = 0.5))
}

#e.g. hvol_plotter(vol_SPY2024,breaks="1 month", title="Realised Volatility - SPY 2024")
