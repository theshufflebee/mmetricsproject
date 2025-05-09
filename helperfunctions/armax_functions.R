# This script contains all functions related implementation of ARMA-X

#Necessary libraries
library(forecast)
library(ggplot2)
library(texreg)

#--------------------------------------------------------------------------------
#-----------------                     1                        -----------------
#--------------------------------------------------------------------------------

# This function creates lags from a variable

#--------------------------------------------------------------------------------

lag_creator <- function(xreg, nb.lags = 3, varname = "xreg") {
  xreg_lags <- embed(xreg, nb.lags + 1)
  colnames(xreg_lags) <- paste0(varname, "_lag_", 0:nb.lags)
  return(xreg_lags)
}




#--------------------------------------------------------------------------------
#-----------------                     2                        -----------------
#--------------------------------------------------------------------------------

# This function uses the JP-Renne way of selecting the number of lags

#--------------------------------------------------------------------------------


lag_selector <- function(y, xreg, nb.lags = 3, type = "text") {
  #name of the xreg variable
  xreg_name <- deparse(substitute(xreg))
  
  #first create lags
  xreg_lags <- lag_creator(xreg, nb.lags, varname = xreg_name)
  
  #align y to match lagged xreg
  y <- tail(y, nrow(xreg_lags))

  #fit an ARMA(0,0,0) model with lm (with r set above)
  eq <- lm(y ~ xreg_lags)
  
  #compute Newey-West HAC standard errors 
  var.cov.mat <- NeweyWest(eq, lag = nb.lags + 4, prewhite = FALSE)
  robust_se <- sqrt(diag(var.cov.mat))
  
  #output table; significant lags are how many we choose
  stargazer(eq, eq, type = type, header=FALSE,
            column.labels = c("(no HAC)", "(HAC)"), keep.stat = "n",
            se = list(NULL, robust_se), no.space = TRUE)
  
  return(invisible(eq))
}


#e.g. lag_selector(y=armax_data$r_vol_h,xreg=armax_data$dummy,nb.lags=12)



#--------------------------------------------------------------------------------
#-----------------                     3                        -----------------
#--------------------------------------------------------------------------------

# This function uses Information Criterions
# to select number of lags (using loops)

#--------------------------------------------------------------------------------

select_armax_ic <- function(y, x, max_p = 3, max_q = 3, 
                             max_r = 5, criterion = "AIC") {
  
  #creating lags according to r
  create_lags <- function(x, r) {
    if (is.null(dim(x))) {
      x <- matrix(x, ncol = 1)
    }
    
    #applying to each column of Xreg (for each exo variable)
    lagged_list <- lapply(1:ncol(x), function(j) {
      col <- x[, j]
      if (r == 0) return(matrix(col, ncol = 1, dimnames = list(NULL, paste0("X", j, "_Lag_0"))))
      embedded <- embed(col, r + 1)
      colnames(embedded) <- paste0("X", j, "_Lag_", 0:r)
      return(embedded)
    })
    
    #ensure all matrices have the same number of rows
    min_rows <- min(sapply(lagged_list, nrow))
    lagged_list <- lapply(lagged_list, function(mat) tail(mat, min_rows))
    
    do.call(cbind, lagged_list)
  }
  
  #prepare results
  best_model <- NULL
  best_score <- Inf #since minimizing
  best_params <- list(p = NA, q = NA, r = NA)
  scores_by_r <- data.frame(r = integer(), score = numeric())
  
  #first loop for each lag
  for (r in 0:max_r){
    x_lags <- create_lags(x, r)
    y_trimmed <- tail(y, nrow(x_lags))
    best_r_score <- Inf
    
    #second loop for each AR(p)
    for (p in 0:max_p){
      
      #third loop for each MA(q)
      for (q in 0:max_q){
        model <- tryCatch({
          Arima(y_trimmed, order = c(p, 0, q), xreg = x_lags)
        }, error = function(e) NULL)
        
        #option for AIC or BIC
        if (!is.null(model)){
          score <- if (criterion == "BIC") BIC(model) else AIC(model)
          
          #chooses the best score (lowest value of AIC/BIC)
          if (score < best_score){
            best_score <- score
            best_model <- model
            best_params <- list(p = p, q = q, r = r)}
          
          if (score < best_r_score){
            best_r_score <- score}}}}
    scores_by_r <- rbind(scores_by_r, data.frame(r = r, score = best_r_score))
  }
  
  #plot AIC/BIC vs r
  ICplot =  ggplot(scores_by_r, aes(x = r, y = score)) +
    geom_line(color = "steelblue", size = 1.2) +
    geom_point(color = "red", size = 2) +
    labs(title = paste(criterion, "vs Number of Exogenous Lags (r)"),
         x = "r (Number of Lags on Exogenous Variable)", y = criterion) +
    theme_minimal()
  
  #results
  list(
    model = best_model,
    params = best_params,
    score = best_score,
    criterion = criterion,
    scores_by_r = scores_by_r,
    ICplot = ICplot)
}



#e.g. result <- select_armax(armax_data$SPY_vol, armax_data$N, 
#                       max_p = 3, max_q = 3, max_r = 5, criterion = "AIC")

#summary(result$model) 



#--------------------------------------------------------------------------------
#-----------------                     4                        -----------------
#--------------------------------------------------------------------------------

# This function implements an ARMAX model

#--------------------------------------------------------------------------------

armax <- function(y, xreg, nb.lags = 3, max.p = 5, 
                  max.q = 5, max.d = 0, latex=FALSE){
  
  #name of the xreg variable
  xreg_name <- sub(".*\\$", "", deparse(substitute(xreg)))
  
  #first create lags
  xreg_lags <- lag_creator(xreg, nb.lags, varname = xreg_name)
  
  #align y to match lagged xreg
  y <- tail(y, nrow(xreg_lags))

  #find best armax model and fit
  tab = auto.arima(y, xreg = xreg_lags, seasonal = FALSE, 
                   max.p = max.p, max.q = max.q, max.d = max.d,
                    stepwise = FALSE, approximation = FALSE, trace = FALSE)

  #print the result
  if (latex == FALSE) {
    print(screenreg(tab, digits = 4))} else {
    print(texreg(tab, caption = "ARMAX Model Results", 
                 label = "tab:armax", digits = 4))}
  
  return(invisible(tab))
}

  

