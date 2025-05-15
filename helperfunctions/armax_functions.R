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

auto.armax.r <- function(y, x, max_p = 3, max_q = 3, 
                         max_r = 5, criterion = "AIC", latex = TRUE) {
  
  #setup parallel plan to speed up code
  future::plan(future::multisession, multicore=16) 
  
  #clean variable name
  x_name <- sub(".*\\$", "", deparse(substitute(x)))
  
  best_model <- NULL
  best_score <- Inf
  best_params <- list(p = NA, q = NA, r = NA)
  scores_by_r <- numeric(length = max_r + 1)
  
  for (r in 0:max_r) {
    x_lags <- lag_creator(x, nb.lags = r, varname = x_name)
    y_trimmed <- tail(y, nrow(x_lags))
    
    if (length(y_trimmed) <= max_p + max_q + 1) {
      scores_by_r[r + 1] <- NA
      next
    }
    
    # (p, q) combinations
    pq_grid <- expand.grid(p = 0:max_p, q = 0:max_q)
    
    #do combinations in parallel
    results <- future.apply::future_lapply(1:nrow(pq_grid), function(i) {
      p <- pq_grid$p[i]
      q <- pq_grid$q[i]
      
      model <- tryCatch({
        Arima(y_trimmed, order = c(p, 0, q), xreg = x_lags)
      }, error = function(e) NULL)
      
      if (!is.null(model)) {
        score <- if (criterion == "BIC") BIC(model) else AIC(model)
        list(model = model, score = score, p = p, q = q)
      } else {
        NULL
      }
    })
    
    #filter out failed models
    results <- Filter(Negate(is.null), results)
    
    if (length(results) == 0) {
      scores_by_r[r + 1] <- NA
      next
    }
    
    #best for this r
    best_r <- results[[which.min(sapply(results, `[[`, "score"))]]
    scores_by_r[r + 1] <- best_r$score
    
    if (best_r$score < best_score) {
      best_model <- best_r$model
      best_score <- best_r$score
      best_params <- list(p = best_r$p, q = best_r$q, r = r)
    }
  }
  
  #plot AIC/BIC vs r
  scores_df <- data.frame(r = 0:max_r, score = scores_by_r)
  ICplot <- ggplot(scores_df, aes(x = r, y = score)) +
    geom_line(color = "steelblue", size = 1.2) +
    geom_point(color = "red", size = 2) +
    labs(title = paste(criterion, "vs Number of Exogenous Lags (r)"),
         x = "r (Number of Lags on Exogenous Variable)", y = criterion) +
    theme_minimal()
  
  #print 
  if (!is.null(best_model)) {
    if (latex) {
      print(texreg(best_model,
                   caption = paste("ARMAX selected by", criterion),
                   label = "tab:armax_select", digits = 4))
    } else {
      print(screenreg(best_model, digits = 4))
    }
  } else {
    warning("No valid ARMAX model was found.")
  }
  
  return(list(
    model = best_model,
    params = best_params,
    score = best_score,
    criterion = criterion,
    scores_by_r = scores_df,
    ICplot = ICplot))}



#-------------------------------------------------------------------------------
#-----------------                     4                        ----------------
#-------------------------------------------------------------------------------

# This function implements an ARMAX model

#-------------------------------------------------------------------------------

#this first one finds the lowest AIC value for a certain number of xlags

auto.armax <- function(y, xreg, nb.lags = 3, max.p = 5, 
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

  
#-------------------------------------------------------------------------------

#this one just fits a model

armax <- function(y, xreg, nb.lags = 3, p=5, q=0, d=0, latex=FALSE){
  
  #name of the xreg variable
  xreg_name <- sub(".*\\$", "", deparse(substitute(xreg)))
  
  #first create lags
  xreg_lags <- lag_creator(xreg, nb.lags, varname = xreg_name)
  
  #align y to match lagged xreg
  y <- tail(y, nrow(xreg_lags))
  
  #find best armax model and fit
  tab = Arima(y, xreg = xreg_lags, order = c(p,d,q), seasonal = F)
  
  #print the result
  if (latex == FALSE) {
    print(screenreg(tab, digits = 4))} else {
      print(texreg(tab, caption = "ARMAX Model Results", 
                   label = "tab:armax", digits = 4))}
  
  return(invisible(tab))
}



#--------------------------------------------------------------------------------
#-----------------                     5                        -----------------
#--------------------------------------------------------------------------------

# Function for plotting IRFs from ARMAX functions above

#--------------------------------------------------------------------------------

irf.plot = function(model,T, title="ARMA-X IRF"){
  
    phi = model$model$phi
    sigma = sqrt(model$sigma2)
    coefficients = coef(model)
    p = model$arma[1]
    q = model$arma[2]
    beta = coefficients[(p+q+2):length(coefficients)]
    
    
    IRFdata <- sim.arma(c=0,phi=phi,theta=beta,sigma=sigma,
                         T=T,y.0=rep(0,length(phi)),
                         nb.sim=1,make.IRF=1)
    
    IRF = cbind.data.frame(1:T,IRFdata)
    colnames(IRF) = c("Period","IRF")
    
    plot = ggplot(IRF,aes(x = Period, y = IRFdata)) +
            geom_line(color = "steelblue", size = 1.2) +
            labs(title = title, y="Hours after shock") +
            theme_minimal(base_size = 14) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(face = "bold", hjust = 0.5))

  return(plot)}


#--------------------------------------------------------------------------------
#-----------------                     *                        -----------------
#--------------------------------------------------------------------------------

# IRF code from JPR notes (appendix 8.5.2)

#--------------------------------------------------------------------------------

irf.function <- function(THETA){
  c <- THETA[1]
  phi <- THETA[2:(p+1)]
  if(q>0){
    theta <- c(1,THETA[(1+p+1):(1+p+q)])
  }else{theta <- 1}
  sigma <- THETA[1+p+q+1]
  r <- dim(Matrix.of.Exog)[2] - 1
  beta <- THETA[(1+p+q+1+1):(1+p+q+1+(r+1))]
  
  irf <- sim.arma(0,phi,beta,sigma=sd(Ramey$ED3_TC,na.rm=TRUE),T=60,
                  y.0=rep(0,length(x$phi)),nb.sim=1,make.IRF=1,
                  X=NaN,beta=NaN)
  return(irf)}
