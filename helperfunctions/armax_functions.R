lag_creator <- function(xreg,nb.lags=3){
  
  #create the lags
  xreg_lags <- embed(xreg, nb.lags + 1)
  #rename
  colnames(xreg_lags) <- paste0("_", 0:nb.lags)
  return(xreg_lags)
  
}



lag_selector <- function(y,xreg,nb.lags=3,type="text"){
  
  #first create lags
  xreg_lags = lag_creator(xreg,nb.lags)
  #align y to match rows (for lag)
  y_aligned <- tail(y, nrow(xreg_lags))
  # fit an ARMA(0,0,0) model with lm (with r set above)
  eq <- lm(y_aligned ~ xreg_lags)
  #compute Newey-West HAC standard errors for count
  var.cov.mat <- NeweyWest(eq, lag = nb.lags+4, prewhite = FALSE)
  robust_se <- sqrt(diag(var.cov.mat))
  #output table; significant lags are how many we choose
  stargazer(eq, eq, type = type,
            column.labels = c("(no HAC)", "(HAC)"), keep.stat = "n",
            se = list(NULL, robust_se), no.space = TRUE)
}


