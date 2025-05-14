install.packages("TSA")
install.packages("aTSA")
install.packages("rugarch") #univariate GARCH
install.packages("rmgarch") #multivarate
install.packages("quantmod")
install.packages("fGarch")
install.packages("FinTS")
install.packages("texreg")
install.packages("forecast")


k

library("quantmod")
library("TSA")
library("aTSA")
library("rugarch")
library("rmgarch")
library("ggplot2")
library("tibble")
library("dplyr")
library("lubridate")
library(tibble)
library(tidyverse)
library(knitr)
library("fGarch")
library(FinTS)
library(kableExtra)
library(writexl)
library(texreg)
library("purrr")
library("forecast")





rm(list=ls())


#An Arch model has an added value relative to arma model which is that 
#it took into account the volatility of a time series
#So bisecally it said that if I have lots of volatility today, 
#it is probable that I will have also lot of volatility tomorrow


#in arch model there is 2 things that need to be specified :
#mean model (if it is arma(1,0) or arma(0,1) or else)
#variance model (what is the persistency of the variance of  the error)
#So the process will be define as ARMA(p,q)-ARCH(r) where r is the number of lag of previous outcome (y)


#A typical ARCH model will be 

$$y_{t} = \epsilon_{t} * sqrt(\omega +\alpha_{1} * y_{t-1}^2) = \epsilon_{t} * \theta_{t}$$


#Here we see that the volatility (theta), depends on previous result of y_{t}
#We will end up with a model that encompass a low persistency of previous volatility
set.seed(1235677)
garch01.sim=garch.sim(alpha=c(.01,.8),beta=c(0), n=50)
plot(garch01.sim,type='l', main='',ylab=expression(r[t]),xlab='t')
abline(h = 0, col = "red")


#For GARCH model, we assume that the persistency of volatility is now low, but persist longer
#So the process will be define as ARMA(p,q)-ARCH(r,m) where m is the number of lag of previous volatility (theta)

$$y_{t} = \epsilon_{t} * sqrt(\omega +\alpha_{1} * y_{t-1}^2 + \beta_{1}*\theta_{t-1}) = \epsilon_{t} * \theta_{t}


set.seed(1235677)
garch02.sim=garch.sim(alpha=c(.01,.8), beta =c(0.18),n=50)
plot(garch02.sim,type='l', main='',ylab=expression(r[t]),xlab='t')
abline(h = 0, col = "red")



#--------------------------------------------------
#univariate GARCH.
data <- read.csv("data/market_data/SPY_24_25.csv")
view(data)


#first let's define our variable
SPY_weighted <- data %>%
  mutate(
    timestamp = dmy_hm(timestamp),                  
    date_only = as.Date(timestamp),                 
    Open = as.numeric(Open),                        
    Close = as.numeric(Close),
    Volume = as.numeric(Volume)
  ) %>%
  group_by(date_only) %>%
  mutate(
    volume_total = sum(Volume, na.rm = TRUE),
    weight = Volume / volume_total,
    weighted_close = weight * Close
  ) %>%
  summarise(
    weighted_mean_close = sum(weighted_close, na.rm = TRUE),
    weighted_var_close = sum(weight * (Close - sum(weighted_close, na.rm = TRUE))^2, na.rm = TRUE)
  )

view(SPY_weighted)



vSPY2 <- c()

for(i in 2:length(SPY_weighted$weighted_mean_close)) {
  growth = log(SPY_weighted$weighted_mean_close[i] / SPY_weighted$weighted_mean_close[i - 1])
  vSPY2 <- c(vSPY2, growth)
}
print(vSPY2)
mean(vSPY2)
view(vSPY2)


vSPY2_with_date <- data.frame(
  date = SPY_weighted$date_only[2:322],
  log_return = vSPY2
)
view(vSPY2_with_date)


ggplot(vSPY2_with_date, aes(x = date, y = log_return)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred", size = 1.5)




#or



r.vol_day_hour <- function(data) {
  data$timestamp <- as.POSIXct(data$timestamp)
  data$hour <- format(data$timestamp, "%H")
  hours <- unique(data$hour)
  r_vol <- numeric(length(hours))
  
  for (h in seq_along(hours)) {
    subset_data <- data[data$hour == hours[h], ]
    p_h <- subset_data$Close
    delta_price <- diff(p_h)
    #delta_price <- diff(log(p_h))
    delta_price_sqr = delta_price^2
    r_vol[h] <- mean(delta_price_sqr, na.rm = TRUE)
  }
  
  return(r_vol)
}


r.vol_hourly <- function(data, merge = TRUE) {
  original_colnames <- colnames(data)
  data$timestamp <- as.POSIXct(data$timestamp)
  data$Date <- as.Date(data$timestamp)
  days <- unique(data$Date)
  
  r_vol_list <- list()
  
  for (d in seq_along(days)) { #loop for each day
    
    #select data for that day
    daydata <- data[data$Date == days[d], ]
    
    #isolate hours and calculate volatility
    hours <- unique(format(daydata$timestamp, "%Y-%m-%d %H:00:00"))
    vol <- r.vol_day_hour(daydata)
    
    #add to list
    r_vol_day <- data.frame(timestamp = as.POSIXct(hours), r_vol_h = vol)
    r_vol_list[[d]] <- r_vol_day
  }
  r_vol_y <- do.call(rbind, r_vol_list)
  
  #option to merge calculated volatility with original data
  if (merge == TRUE) {
    
    #format for hours 
    data$timestamp_hour <- as.POSIXct(format(data$timestamp, "%Y-%m-%d %H:00:00"))
    
    #merge by matching the hours
    merged_data <- merge(data, r_vol_y, by.x = "timestamp_hour", by.y = "timestamp", all.x = TRUE)
    
    #make sure its ordered by minute (merging by hour messes it up)
    merged_data <- merged_data[order(merged_data$timestamp), ]
    
    #get original column order
    final_data <- merged_data[, c(original_colnames, "r_vol_h")]
    return(final_data)
  } else {
    colnames(r_vol_y)[1] <- "timestamp"
    return(r_vol_y)
  }
}

vSPY3 =r.vol_hourly(data)$r_vol_h
view(vSPY3)


#or 

price = data$Close

#or

return = data %>%
  mutate(
    timestamp = dmy_hm(timestamp),                  
    date_only = as.Date(timestamp),                 
    Open = as.numeric(Open),                        
    Close = as.numeric(Close),
    Volume = as.numeric(Volume)
  ) %>%
  group_by(date_only) %>%
  mutate(
    returns = mean(diff(Close))
  )
ggplot (return, aes(x=date_only, y= returns)) +
        geom_line(color = "steelblue") +
          labs(title = "Daily Returns", x = "Date", y = "Returns") +
          theme_minimal()

return = return$returns

#As we can see, Big price changes
#— up or down — tend to happen within a close period. This is
#what is commonly called ‘volatility clustering’ and it's one sign
#that the use of GARCH is relevant



#1) Model Specification

#let's see if it encompass MA or/and AR process component
acf(return)
pacf(return)
#For financial returns, we expect to find low values of
#positive or negative correlations. In other words, past financial
#returns have very low explanatory power over future returns.


#Before we estimate a GARCH model, we need to make
#sure the effect exists in the dataset. For that, we use the Lagrange
#multiplier (LM) test for ARCH effects 
#it works by regressing the squared errors on its lags and testing the hypothesis
#that all coefficients of the lagged regression are equal to zero.
do_arch_test <- function(x, max_lag = 5) {
  require(FinTS)
  require(tidyverse)
  
  do_single_arch <- function(x, used_lag)  {
    test_out <- FinTS::ArchTest(x, lags = used_lag)
    
    res_out <- tibble(Lag = used_lag,
                      `LMStatistic` = test_out$statistic, 
                      `pvalue` = test_out$p.value)
  }
  
  tab_out <- bind_rows(map(1:max_lag,.f = do_single_arch, x = x))
  
  return(tab_out)
}


table <- do_arch_test(x = return, max_lag = 5)
table

#we want pvalue lower than 1 and closer to 0



#Model Specification
ug_spec =ugarchspec()
ug_spec

#we can also give to r some specification that we want to have in the model
ug_spec2 = ugarchspec(mean.model=list(armaOrder=c(1,3))) #for changing mean model and having arma(1,0)
ug_spec2





#2) Model Estimation
ugfit =ugarchfit(spec = ug_spec2, data = vSPY3)
ugfit

#coefficients should be statistically
#mu mu should be positive (financial property)
#Garch(1,1), sum of alpha and beta should be lower than one 
#if more (equation in lectures slide ch.8)

#for EGARCH and CJG-GARCH if gamma1 is positive and statistically significant, it means that
#shock have asymetric effect meaning, i.e., the persistency can be higher if there is 
#an increase of volatility rather than a decrease
#clearly this showes how the volatility reacts differently to bad news  
#with respect to good news. Thus, when bad news hit the
#market and returns are negative, volatility increases strongly


#Now we have 2 types of slots of variable (@fit or @model)
names(ugfit@model)
names(ugfit@fit)

ug_var = ugfit@fit$var #volatility 
ug_res = (ugfit@fit$residuals) #volatility of the error
ug_res2 = ug_res^2

plot(ug_res2, type="l")
lines(ug_var, col="green")






#Different type of GARCH model will be tested here 



models_to_estimate <- c('sGARCH', 'eGARCH', 'gjrGARCH')
distribution_to_estimate = "norm"
arch_lag = 1 #persistency of previous outcome
garch_lag = 1 #persistency of previous volatility
ar_lag=1
ma_lag=1

# get all combinations of models
df_grid <- expand_grid(ar_lag,
                       ma_lag,
                       arch_lag,
                       garch_lag,
                       models_to_estimate,
                       distribution_to_estimate)


estimate_garch <- function(ar_lag,
                           ma_lag,
                           arch_lag,
                           garch_lag,
                           models_to_estimate,
                           distribution_to_estimate) {
  
  message('Estimating ARMA(',ar_lag,',', ma_lag, ')', '-',
          models_to_estimate, '(', arch_lag, ',', garch_lag, ') ', 
          'dist = ', distribution_to_estimate)
  
  # estimate model
  my_spec <- ugarchspec(variance.model = list(model = models_to_estimate,
                                              garchOrder = c(arch_lag, 
                                                             garch_lag)),
                        mean.model = list(armaOrder = c(ar_lag,
                                                        ma_lag)), 
                        distribution.model = distribution_to_estimate)
  
  my_garch <- ugarchfit(spec = my_spec, data = vSPY3) #change data here
  
  return(my_garch)
}

# estimate all models
l_args <- as.list(df_grid)
l_models <- pmap(.l = l_args, .f = estimate_garch)

# make sure dir "tabs" exists
if (!dir.exists('tabs')) dir.create('tabs')



extract.rugarch <- function(fit, 
                            include.rsquared = FALSE, 
                            include.loglike = TRUE, 
                            include.aic = TRUE, 
                            include.bic = TRUE) {
  
  require(texreg)
  
  # extract coefficient table from fit:
  coefnames <- rownames(as.data.frame(fit@fit$coef))
  coefs <- fit@fit$coef
  se <- as.vector(fit@fit$matcoef[, c(2)])
  pvalues <-  as.vector(fit@fit$matcoef[, c(4)])       # numeric vector with p-values
  
  # create empty GOF vectors and subsequently add GOF statistics from model:
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()
  if (include.loglike == TRUE) {
    loglike <- fit@fit$LLH
    gof <- c(gof, loglike)
    gof.names <- c(gof.names, "Log likelihood")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  if (include.aic == TRUE) {
    aic <- infocriteria(fit)[c(1)]
    gof <- c(gof, aic)
    gof.names <- c(gof.names, "AIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  
  if (include.bic == TRUE) {
    bic <- infocriteria(fit)[c(2)]
    gof <- c(gof, bic)
    gof.names <- c(gof.names, "BIC")
    gof.decimal <- c(gof.decimal, TRUE)
  }
  
  # include distribution and type variance
  # browser()
  #   variance_model <- fit@model$modeldesc$vmodel
  #   type_dist <- fit@model$modeldesc$distribution
  #   gof <- c(gof, variance_model, type_dist)
  #   gof.names <- c(gof.names, "Variance Model", 'Distribution')
  #   gof.decimal <- c(gof.decimal, TRUE, TRUE)
  
  # create texreg object:
  tr <- createTexreg(
    coef.names = coefnames, 
    coef = coefs,
    se = se,
    pvalues = pvalues, 
    gof.names = gof.names, 
    gof = gof, 
    gof.decimal = gof.decimal
  )
  return(tr)
}



# reformat models for texreg
l_models <- map(l_models, extract.rugarch, include.rsquared = FALSE)

# write custom row
custom_row <- list('Variance Model' = df_grid$models_to_estimate,
                   'Distribution' = df_grid$distribution_to_estimate)
custom_names <- paste0('Model ', 1:length(l_models))

# print to screen
screenreg(l_models,
          custom.gof.rows = custom_row,
          custom.model.names = custom_names, 
          digits = 3)




#3) finding the best model for our GARCH
#set x = data
x = return
max_lag_AR <- 1 
max_lag_MA <- 1 
max_lag_ARCH <- 2
max_lag_GARCH <- 2 
dist_to_use <- c('norm','std') #or c('norm','std')


#change type_model if other than sGARCH

do_single_garch <- function(x, 
                            type_model, 
                            type_dist, 
                            lag_ar, 
                            lag_ma, 
                            lag_arch, 
                            lag_garch) {
  require(rugarch)
  
  
  spec = ugarchspec(variance.model = list(model =  type_model, 
                                          garchOrder = c(lag_arch, lag_garch)),
                    mean.model = list(armaOrder = c(lag_ar, lag_ma)),
                    distribution = type_dist)
  
  message('Estimating ARMA(',lag_ar, ',', lag_ma,')-',
          type_model, '(', lag_arch, ',', lag_garch, ')', 
          ' dist = ', type_dist,
          appendLF = FALSE)
  
  try({
    my_rugarch <- list()
    my_rugarch <- ugarchfit(spec = spec, data = x)
  })
  
  if (!is.null(coef(my_rugarch))) {
    message('\tDone')
    
    AIC <- rugarch::infocriteria(my_rugarch)[1]
    BIC <- rugarch::infocriteria(my_rugarch)[2]
  } else {
    message('\tEstimation failed..')
    
    AIC <- NA
    BIC <- NA
  }
  
  est_tab <- tibble(lag_ar, 
                    lag_ma,
                    lag_arch,
                    lag_garch,
                    AIC =  AIC,
                    BIC = BIC,
                    type_model = type_model,
                    type_dist,
                    model_name = paste0('ARMA(', lag_ar, ',', lag_ma, ')+',
                                        type_model, '(', lag_arch, ',', lag_garch, ') ',
                                        type_dist) ) 
  
  return(est_tab)
}


find_best_arch_model <- function(x, 
                                 type_models, 
                                 dist_to_use,
                                 max_lag_AR,
                                 max_lag_MA,
                                 max_lag_ARCH,
                                 max_lag_GARCH) {
  
  require(tidyr)
  
  df_grid <- expand_grid(type_models = type_models,
                         dist_to_use = dist_to_use,
                         arma_lag = 0:max_lag_AR,
                         ma_lag = 0:max_lag_MA,
                         arch_lag = 1:max_lag_ARCH,
                         garch_lag = 1:max_lag_GARCH)
  
  
  l_out <- pmap(.l = list(x = rep(list(x), nrow(df_grid)), 
                          type_model = df_grid$type_models,
                          type_dist = df_grid$dist_to_use,
                          lag_ar = df_grid$arma_lag,
                          lag_ma = df_grid$ma_lag,
                          lag_arch = df_grid$arch_lag,
                          lag_garch  = df_grid$garch_lag),
                do_single_garch)
  
  tab_out <- bind_rows(l_out)
  
  # find by AIC
  idx <- which.min(tab_out$AIC)
  best_aic <- tab_out[idx, ]
  
  # find by BIC
  idx <- which.min(tab_out$BIC)
  best_bic <- tab_out[idx, ]
  
  l_out <- list(best_aic = best_aic,
                best_bic = best_bic,
                tab_out = tab_out)
  
  return(l_out)
}

out <- find_best_arch_model(x, 
                            type_models = models_to_estimate,
                            dist_to_use = dist_to_use,
                            max_lag_AR = max_lag_AR,
                            max_lag_MA = max_lag_MA,
                            max_lag_ARCH = max_lag_ARCH,
                            max_lag_GARCH = max_lag_GARCH)
tab_out <- out$tab_out

# pivot table to long format (better for plotting)
df_long <- tidyr::pivot_longer(data = tab_out %>%
                                 select(model_name,
                                        type_model,
                                        type_dist,
                                        AIC, BIC),  cols = c('AIC', 'BIC'))

models_names <- unique(df_long$model_name)
best_models <- c(tab_out$model_name[which.min(tab_out$AIC)],
                 tab_out$model_name[which.min(tab_out$BIC)])

# figure out where is the best model
df_long <- df_long %>%
  mutate(order_model = if_else(model_name %in% best_models, 'Best Model', 'Not Best Model') ) %>%
  na.omit()

# make table with best models
df_best_models <- df_long %>%
  group_by(name) %>%
  summarise(model_name = model_name[which.min(value)],
            value = value[which.min(value)],
            type_model = type_model[which.min(value)])

# plot results
p1 <- ggplot(df_long %>%
               arrange(type_model), 
             aes(x = reorder(model_name, 
                             order(type_model)),
                 y = value, 
                 shape = type_dist,
                 color = type_model)) + 
  geom_point(size = 3.5, alpha = 0.65) + 
  coord_flip() + 
  facet_wrap(~name, scales = 'free_x') + 
  geom_point(data = df_best_models, mapping = aes(x = reorder(model_name, 
                                                              order(type_model)),
                                                  y = value), 
             color = 'blue', size = 5, shape = 8) +
  labs(title = 'Selecting Garch Models by Fitness Criteria', 
       subtitle = 'The best model is the one with lowest AIC or BIC (with star)',
       x = '',
       y = 'Value of Fitness Criteria',
       shape = 'Type of Dist.',
       color = 'Type of Model') + 
  theme(legend.position = "right")
#normal size is width = 5, height = 7. Maximum size is width = 49 and height = 49
ggsave("garch_model_selection.jpg", plot = p1, path = "C:/Users/th9fe/OneDrive/Bureau/Econometric", width = 9, height = 10, limitsize = TRUE)



# estimate best garch model by BIC 
best_spec = ugarchspec(variance.model = list(model =  out$best_bic$type_model, 
                                             garchOrder = c(out$best_bic$lag_arch,
                                                            out$best_bic$lag_garch)),
                       mean.model = list(armaOrder = c(out$best_bic$lag_ar, 
                                                       out$best_bic$lag_ma)),
                       distribution = 'std')

my_best_garch <- ugarchfit(spec = best_spec, 
                           data = x)




#4) Forcasting (not important)
ugfor = ugarchforecast(ugfit, n.ahead = 10)
ugfor


ug_f = ugfor@forecast$sigmaFor
plot(ug_f, type="l")


ug_var_t = c(tail(ug_var,20), rep(NA, 10)) #get last 20 observation
ug_res2_t = c(tail(ug_res2, 20), rep(NA,10))
ug_f = c(rep(NA, 20), (ug_f)^2)

plot(ug_res2_t, type="l")
lines(ug_f, col="orange") #forecast
lines(ug_var_t, col="green") #last 20 observations


#----------------------------------------
#Multivariate




#---------------------------------

#ALSO
#One possible critique to ARCH-GARCH family is
#that it models the conditional variance as a linear function
#of the squared past innovations and its implicit symmetry of
#impact, where a positive shock affects volatility the same way as
#a negative shock. The exponential GARCH model (EGARCH) allows asymmetric effects depending on the
#sign of the random innovation (error term). This follows the
#idea that volatility may rise in response to ‘bad news’ and can be
#reduced after ‘good news (we can put a coefficient that say that volatility tends to increase
#more after negative returns than in positive returns )
#GJR-GARCH model do the same but in a simpler way


#----------------------------------------------
#adding exogenous shock 

#There is 2 ways of integrating exogenous shock to a GARCH

#1. TWO-STEP APPROACH 
#First define the conditional var of y_{t}
#This conditional var is thus conditional on 
# the information in the variable of interest 
#itself, particularly the past shocks and the past values of conditional variance
#In the second step, additional variables are brought in to explain the variation in the conditional variance.

#2 ONE-STEP APPROACH
#the conditional variance is defined and estimated as depending 
#on the exogenous regressors in addition to the past shocks and past values of the conditional variance.

#These two approaches are fundamentally different in how the conditional variance is defined. It is up 
#to you to decide whether you are interested in defining the conditional variance with conditioning only on past shocks 
#and past own values or also on the exogenous variables. The difference between the two should be justified by 
#subject-matter arguments rather than statistical ones.

#two-step approach seems to have the following advantage over the one-step approach: you can assess the goodness of fit 
#of the equation where the fitted conditional variance is being explained by the exogenous regressors. There, you can 
#see how much of the variation these variables explain. But actually you can also run a similar regression in the case of 
#the one-step approach, too.

Trump = rnorm(n= 321, mean = 1, sd = 0.5)
length(vSPY2) == length(Trump)
as.data.frame(Trump)
TrumpM = matrix(c(Trump), ncol=1)

models_to_estimatex <- c('sGARCH')
distribution_to_estimatex = "norm"
archx_lag = 1 #persistency of previous outcome
garchx_lag = 1 #persistency of previous volatility
arx_lag=0
max_lag=0



my_specx <- ugarchspec(variance.model = list(model = models_to_estimate,
                                            garchOrder = c(arch_lag, 
                                                             garch_lag)),
                        mean.model = list(armaOrder = c(ar_lag,
                                                        ma_lag), external.regressors=matrix(c(df_merged$dummy), ncol=1)), 
                        distribution.model = distribution_to_estimate)
  
my_garchx <- ugarchfit(spec = my_specx, data = df_merged$mean_hourly_return)


Trump2 = matrix(c(-0.0001 + vSPY2[2:321], 0.001), ncol=1)
