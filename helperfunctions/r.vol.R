# This function computes a day's realised volatility

#Necessary libraries
library(dplyr)

#--------------------------------------------------------------------------------

#t data is t=day's data

r.vol = function(t_data){
  p_t = t_data$close
  p_t_1 <- lag(p_t, 1)
  delta_price = p_t_1 - p_t
  delta_price_sqr = delta_price^2
  v_t = sum(na.omit(delta_price_sqr)) / length(na.omit(delta_price))
  return(v_t)
}

#example: r.vol(day_SPY_0402)
#outputs the realised volatility for the 2nd of April