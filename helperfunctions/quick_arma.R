# This function provides a way to quickly check ARMA specification of time-series
# Executes acf,pacf,regressions and checks residual autocorrelation

#Necessary libraries
library(huxtable)
library(jtools) #for tables
library(lmtest) #for coef tests

#--------------------------------------------------------------------------------

#input a time series as data
#p,d,q as in arima function

quick_arma = function(data,p,d,q){ 
  acf(data$close)
  pacf(data$close)
  names = c(paste("AR",p,sep="-"),paste("AR",(p+1),sep="-"),paste("AR",(p+2),sep="-"))
  
  AR1 = arima(data$close,c(p,d,q),method="ML")
  AR2 = arima(data$close,c((p+1),d,q),method="ML")
  AR3 = arima(data$close,c((p+2),d,q),method="ML")
  table1 = export_summs(AR1,AR2,AR3, model.names = names, digits = 4)
  caption(table1) <- "AR Estimations"
  set_width(table1, 0.8)
  print(table1)
  
  names2 = paste(names,"Residuals")
  AR1res = as.numeric(AR1$residuals)
  REG1res_lagged <- lag(AR1res, 1)
  iidcheck1 = lm(AR1res ~ REG1res_lagged)
  AR2res = as.numeric(AR2$residuals)
  REG2res_lagged <- lag(AR2res, 1)
  iidcheck2 = lm(AR2res ~ REG2res_lagged)
  AR3res = as.numeric(AR3$residuals)
  REG3res_lagged <- lag(AR3res, 1)
  iidcheck3 = lm(AR3res ~ REG3res_lagged)
  table2 = export_summs(iidcheck1,iidcheck2,iidcheck3, 
                        model.names = names2, digits = 4)
  caption(table2) <- "Checking Residuals"
  set_width(table2, 0.8)
  print(table2)
  
  #return(list(table1,table2))
  
}


#example: quick_arma(day_ONEQ_0409,2,0,0) 
#tests ARMA(2,0,0) specification on intraday ONEQ data
