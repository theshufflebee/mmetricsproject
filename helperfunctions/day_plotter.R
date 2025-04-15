# This function plots intraday financial data at 1-min intervals

#Necessary libraries
library(ggplot2)

#--------------------------------------------------------------------------------

#input data 
#input title as string

day_plotter = function(data,title){
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

#example: day_plotter(day_ONEQ_0409,"ONEQ Price on April 9th")
