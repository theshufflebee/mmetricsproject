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
require(tinytex) #LaTeX
require(ggplot2) #plots
require(AEC) #JP-Renne functions
require(AER) #NW formula
require(forecast) #time series stuff
require(expm) #matrix exponents
require(here) #directory finder
require(stringr) # analysis of strings, important for the detection in tweets
require(dplyr) #data management
require(lubridate) #data dates management
require(zoo) #for lagging
require(jtools) #tables
require(huxtable) #tables
require(lmtest) #reg tests
require(vroom) #for loading data
require(data.table) #for data filtering
require(sysid) #for ARMA-X modeling
require(sandwhich) #regression errors
require(stargazer) #nice reg tables
require(tidytext) #text mining
require(textstem) #lemmatization
require(quanteda) #tokenization






rm(list=ls())

data <- read.csv("data/market_data/SPY_24_25.csv")

#load helper functions
source(here("helperfunctions/data_loaders.R"))
source(here("helperfunctions/date_selector.R"))
source(here("helperfunctions/plotters.R"))
source(here("helperfunctions/quick_arma.R"))
source(here("helperfunctions/r.vol_calculators.R"))
source(here("helperfunctions/truths_cleaning_function.R"))
source(here("helperfunctions/select_armax.R"))


#Financial data

#return by hour (fonctionne pas lol mais minute oui sa mère la tchoin) #from 2.1.2024
ret = data %>%
  mutate(
    timestamp = dmy_hm(timestamp),
    date = as.Date(timestamp),
    hour = hour(timestamp)
  ) %>%
  arrange(timestamp) %>%
  mutate(return = log(Close/lag(Close))) %>%  #better way diff(log(prices), lag=1)
  group_by(date, hour) %>%
  summarise(mean_hourly_return = mean(return, na.rm = TRUE), .groups = "drop")

ret2 <- ret %>%
  mutate(datetime = as.POSIXct(paste(date, hour), format = "%Y-%m-%d %H", tz = "UTC")) %>%
  select(datetime, mean_hourly_return)


ggplot (SPY, aes(x=timestamp, y= mean_minute_return)) +
  geom_line(color = "steelblue") +
  labs(title = "hourly Returns", x = "Date", y = "Returns") +
  theme_minimal()

#or 


#minute return
ret3 <- data %>%
  mutate(
    timestamp = dmy_hm(timestamp),
    date = as.Date(timestamp),
    hour = hour(timestamp),
    minute = minute(timestamp),
  ) %>%
  arrange(timestamp) %>%
  mutate(return = 100*log(Close/lag(Close))) %>%  #better way diff(log(prices), lag=1)
  group_by(date, hour, minute) %>%
  summarise(mean_minute_return = mean(return, na.rm = TRUE), .groups = "drop")

ret4 <- ret3 %>%
  mutate(datetime = as.POSIXct(paste(date, hour, minute), format = "%Y-%m-%d %H%M", tz = "UTC")) %>%
  select(datetime, mean_minute_return) %>%
  mutate(mean_minute_return = if_else(is.na(mean_minute_return), 0, mean_minute_return))



#minute return data from 2015 to 2025 (en tt cas 2024 lol)

SPY = read.csv("C:/Users/th9fe/OneDrive/Bureau/Econometric/SPY_15_25.csv")

ret5 <- SPY %>%
  mutate(
    timestamp = dmy_hm(timestamp),
    date = as.Date(timestamp),
    hour = hour(timestamp),
    minute = minute(timestamp),
  ) %>%
  arrange(timestamp) %>%
  mutate(return = 100*log(Close/lag(Close))) %>%  #better way diff(log(prices), lag=1)
  group_by(date, hour, minute) %>%
  summarise(mean_minute_return = mean(return, na.rm = TRUE), .groups = "drop")

ret6 <- ret5 %>%
  mutate(datetime = as.POSIXct(paste(date, hour, minute), format = "%Y-%m-%d %H%M", tz = "UTC")) %>%
  select(datetime, mean_minute_return) %>%
  mutate(mean_minute_return = if_else(is.na(mean_minute_return), 0, mean_minute_return))

ggplot (ret6, aes(x=datetime, y= mean_minute_return)) +
  geom_line(color = "steelblue") +
  labs(title = "hourly Returns", x = "Date", y = "Returns") +
  theme_minimal()



#Trump data

#truthsocial
raw_truths <- read.csv(here("data/political_data", "truths_new.csv"))

#twitter
raw_tweets <- read.csv(here("data/political_data", "tweets.csv"))



## Tweet Cleanup \& Count

tweets = raw_tweets

#only keep original Tweets
tweets <- tweets %>% filter(isRetweet != "t")
tokens <- tokens(tweets$text)
dfm <- dfm(tokens)

#cleanup
tweets = as.data.table(tweets)
names(tweets)[names(tweets) == 'date'] <- 'timestamp'
tweets <- tweets[order(tweets$timestamp, decreasing=T), ]
tweets$timestamp = as.POSIXct(tweets$timestamp,format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

#count by hour
tweet_count = tweets[, .N, by=.(year(timestamp), month(timestamp), 
                                day(timestamp), hour(timestamp), minute(timestamp))] 

#fix timestamp
tweet_count$timestamp = as.POSIXct(sprintf("%04d-%02d-%02d %02d:%02d:00", 
                                           tweet_count$year, tweet_count$month, tweet_count$day, 
                                           tweet_count$hour, tweet_count$minute), format = "%Y-%m-%d %H:%M:00")

tweet_count$timestamp = format(tweet_count$timestamp, tz = "UTC")

#verify the fz 
attr(tweet_count$timestamp, "tzone")


#remove useless columns and reorder by oldest first
tweet_count = select(tweet_count, timestamp, N)
tweet_count = tweet_count[ order(tweet_count$timestamp , decreasing = F ),]



## Truths Cleanup \& Count
truthsbackup <- truths_processer(raw_truths) 

#cleanup
truths = as.data.table(truthsbackup)
names(truths)[names(truths) == 'date_time_parsed'] <- 'timestamp'
truths <- truths[order(truths$timestamp, decreasing=T), ]

#count by hour
truth_count = truths[, .N, by=.(year(timestamp), month(timestamp), 
                                day(timestamp), hour(timestamp), minute(timestamp))] 

#fix timestamp
truth_count$timestamp = as.POSIXct(sprintf("%04d-%02d-%02d %02d:%02d:00", 
                                           truth_count$year, truth_count$month, truth_count$day, 
                                           truth_count$hour, truth_count$minute), format = "%Y-%m-%d %H:%M:00", tz = "UTC")

truth_count$timestamp = format(truth_count$timestamp, tz = "UTC")

#verify the fz 
attr(truth_count$timestamp, "tzone")

                                                                                              
#remove useless columns and reorder by oldest first
truth_count = select(truth_count, timestamp, N)
truth_count = truth_count[ order(truth_count$timestamp , decreasing = F ),]



#reorder and graph
tt_count = rbind(tweet_count,truth_count) #tweets & truths count
tt_count = tt_count %>% mutate(dummy = if_else(N > 0, 1, 0))

ggplot(tt_count, aes(x = timestamp, y = N)) +
  geom_point(color = "#253494", size = 1) +
  scale_x_datetime(date_labels = "%b %Y", date_breaks = "9 month") +
  labs(title = "Trump Social Media Count",
       x = NULL,
       y = "number of tweets/truths") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", hjust = 0.5))


#filter date from when financial data begin 
tt <- tt_count %>%
  filter(timestamp >= as.POSIXct("2024-01-02", tz = "UTC")) %>%
  rename(datetime = timestamp) 
  


tt$datetime <- as.POSIXct(tt$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

tt$datetime <- lubridate::floor_date(tt$datetime, unit = "minute")
ret4$datetime <- lubridate::floor_date(ret4$datetime, unit = "minute")




#link hourly return and Trump tweets
d <- ret4 %>%
  left_join(tt, by = "datetime") %>%      # jointure sur datetime
  mutate(dummy = if_else(is.na(dummy), 0, dummy)) 


#shift dummy and n by one raw (because problem)
ye <-  %>%
  mutate(N_shifted = lag(N, 1)) %>%
  mutate(dummy_shifted = lag(dummy, 1)) %>%
  mutate(dummy_shifted = if_else(is.na(dummy_shifted), 0, dummy_shifted))






#modeling

acf(ret4$mean_minute_return)
pacf(ret4$mean_minute_return)


#LM test
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


table <- do_arch_test(x = ret4$mean_minute_return, max_lag = 5)
table



#adf test (for stationarity of the outcome data)
adf.test(d$mean_minute_return, nlag = NULL, output = TRUE)

#KPSS test (stationarity)
require("tseries")
kpss.test(d$mean_minute_return, null = c("Level", "Trend"), lshort = FALSE)



models_to_estimate <- c('sGARCH', 'eGARCH', 'gjrGARCH')
models_to_estimate2 = c("sGARCH")
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
                                                        ma_lag), external.regressors=matrix(c(d$dummy), ncol=1)), 
                        distribution.model = distribution_to_estimate)
  
  my_garch <- ugarchfit(spec = my_spec, data = d$mean_minute_return) #change data here
  
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


ugfit1 = ugarchspec(variance.model = list(model = models_to_estimate,
                                          garchOrder = c(arch_lag, 
                                                         garch_lag)),
                    mean.model = list(armaOrder = c(ar_lag,
                                                    ma_lag), external.regressors=matrix(c(d$dummy), ncol=1)), 
                    distribution.model = distribution_to_estimate)

garch1 <- ugarchfit(spec = ugfit1, data = d$mean_minute_return)

garch1

names(garch1@model)
names(garch1@fit)

garch1@fit$coef







#3) finding the best model for our GARCH
#set x = data
x = d$mean_minute_return
max_lag_AR <- 0 
max_lag_MA <- 1 
max_lag_ARCH <- 1
max_lag_GARCH <- 1 
dist_to_use <- c('norm','std')
  #c('norm','std') #or c('norm')


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
                    mean.model = list(armaOrder = c(lag_ar, lag_ma), external.regressors=matrix(c(d$dummy), ncol=1)),
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
#normal size is width = 5, height = 7. Here it is the maximum
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
