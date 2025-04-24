#required libraries
library(forecast)
library(ggplot2)

select_armax <- function(y, x, max_p = 3, max_q = 3, max_r = 5, criterion = "AIC") {
  
  #creating lags according to r
  create_lags <- function(x, r) {
    if (r == 0) return(matrix(x, ncol = 1, dimnames = list(NULL, "Lag_0")))
    lagged <- embed(x, r + 1)
    colnames(lagged) <- paste0("Lag_", 0:r)
    return(lagged)
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



#e.g. result <- select_armax(armax_vol$r_vol, armax_vol$N, 
#                       max_p = 3, max_q = 3, max_r = 5, criterion = "AIC")

#summary(result$model) 