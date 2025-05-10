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
require(quanteda) #tokenizationc vfv 
require(vars)



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
require(syuzhet) #sentiment analysis
require(alphavantager)
require(tseries)
av_api_key(Sys.getenv("ALPHAVANTAGE_API_KEY"))



rm(list = ls())


getwd()
#setwd("...") -> set wd at base repo folder

#load helper functions
source(here("helperfunctions/data_loaders.R"))
source(here("helperfunctions/date_selector.R"))
source(here("helperfunctions/plotters.R"))
source(here("helperfunctions/quick_arma.R"))
source(here("helperfunctions/r.vol_calculators.R"))
source(here("helperfunctions/truths_cleaning_function.R"))
source(here("helperfunctions/armax_functions.R"))
source(here("helperfunctions/fulldata_loader.R"))


# 1. Load Political Social Media
#contains posts from Twitter & TruthSocial
social <- read.csv(here("data/mothership", "social.csv"))
social_hourly <- read.csv(here("data/mothership", "socialhourly.csv"))


# 2. Load Financial
#S&P500
#SPY <- read.csv(here("data/mothership", "SPY.csv"))
#STOXX50
#VGK <- read.csv(here("data/mothership", "VGK.csv"))
#CSI 300 (China)
ASHR <- read.csv(here("data/mothership", "ASHR.CSV"))


#make posixct
#SPY$timestamp = as.POSIXct(SPY$timestamp,format = "%Y-%m-%d %H:%M:%S")
#VGK$timestamp = as.POSIXct(VGK$timestamp,format = "%Y-%m-%d %H:%M:%S")
ASHR$timestamp = as.POSIXct(ASHR$timestamp,format = "%Y-%m-%d %H:%M:%S")
social$timestamp = as.POSIXct(social$timestamp,format = "%Y-%m-%d %H:%M:%S")
social_hourly$timestamp = as.POSIXct(social_hourly$timestamp,format = "%Y-%m-%d %H:%M:%S")


#find hourly volatility
#NOTE: this ignores tweets made outside trading hours!!
ASHR_volatility_alltime = dplyr::select(ASHR,timestamp,r_vol_h)
#aggregating per hour
ASHR_volatility_alltime = ASHR_volatility_alltime %>%
  mutate(timestamp = floor_date(timestamp, unit = "hour")) %>%
  distinct(timestamp, .keep_all = TRUE)
#select time period
ASHR_volatility = filter(ASHR_volatility_alltime,
                        between(timestamp,
                                as.Date('2013-01-01'),
                                as.Date('2025-04-10')))

#find count
tweetcount_alltime = dplyr::select(social_hourly,timestamp,N)
#select time period
tweetcount = filter(tweetcount_alltime,
                    between(timestamp,
                            as.Date('2013-01-01'),
                            as.Date('2025-04-10')))

#find dummy
tweetdummy_alltime = dplyr::select(social_hourly,timestamp,dummy)
#select time period
tweetdummy = filter(tweetdummy_alltime,
                    between(timestamp,
                            as.Date('2013-01-01'),
                            as.Date('2025-04-10')))

#find count
tariff_alltime = dplyr::select(social_hourly,timestamp,total_tariff)
#select time period
tariff = filter(tariff_alltime,
                between(timestamp,
                        as.Date('2013-01-01'),
                        as.Date('2025-04-10')))


#find count
trade_alltime = dplyr::select(social_hourly,timestamp,total_trade)
#select time period
trade = filter(trade_alltime,
               between(timestamp,
                       as.Date('2013-01-01'),
                       as.Date('2025-04-10')))




#merge our dependant and independant vars
Vdata = left_join(ASHR_volatility, tweetcount, by="timestamp")
Vdata  = left_join(Vdata , tweetdummy, by="timestamp")
Vdata  = left_join(Vdata , tariff, by="timestamp")
Vdata  = left_join(Vdata , trade, by="timestamp")
#convert NA to zeroes
Vdata $N[is.na(Vdata $N)] = 0
Vdata $dummy[is.na(Vdata $dummy)] = 0
Vdata $total_tariff[is.na(Vdata $total_tariff)] = 0
Vdata $total_trade[is.na(Vdata $total_trade)] = 0

Vdata <- Vdata %>%
  filter(!is.na(r_vol_h))

#------------------------------------------------------------------------------------------------

#check the data  
##stationarity

###acf/pacf
acf(Vdata$r_vol_h)
pacf(Vdata$r_vol_h)

acf(Vdata$dummy)
pacf(Vdata$dummy)

####adf test (for stationarity of the outcome data) 
####p-value test H0, the probability that the process is not stationary
adf.test(Vdata$r_vol_h, k = 80)
adf.test(Vdata$dummy, k = 16)



#series
pro_v = ts(Vdata$r_vol_h)
pro_t_dummy = ts(Vdata$dummy)

autoplot(pro_v)
autoplot(pro_t_dummy)




#Estimate the Process
##with dummy
y = cbind(Vdata$r_vol_h, Vdata$dummy)

y_lag = VARselect(y, lag.max = 28)
y_lag
y_list = list(y_lag)


#dummy
est.VAR <- VAR(y,p=28)
summary(est.VAR)

#-----------------------------------------------------------------------------------------------
#bunch of test 

##serial correlation
serial = serial.test(est.VAR, lags.pt = 20, type = "PT.asymptotic")
serial

#y1 is vol ; y2 is tweet (dummy or N)
plot(serial, names = "y1")
plot(serial, names = "y2")
###H0 test if there is no serial correlation



#heteroscedasticity
hete = arch.test(est.VAR, lags.multi = 80, multivariate.only = TRUE)
hete
##again, h0 test if there is no heteroscedasticity



#Distribution of residuals
norm = normality.test(est.VAR, multivariate.only = TRUE)
norm
##first test tests the dirtribution
##second one tests the skewness
##third one test kurtosis
##all h0 test if there is not...



#Structural breaks in the errors (stability)
stru2 = stability(est.VAR, type ="OLS-CUSUM") #OLS-CUSUM or Rec-CUSUM
plot(stru2)
#--------------------------------------------------------------------------------------------
#SVAR

bmat = matrix(c(1, 0, NA, 1), 2)
SVAR = SVAR(est.VAR, estmethod = c("scoring", "direct"), Amat = NULL, Bmat = bmat)
summary(SVAR)

IRF <- irf(SVAR, response = "y1", impulse = "y2", 
           n.ahead = 80, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(IRF)


#granger causality
#we want the smaller p-value => h0 test if there is no causality
colnames(y)[1:2] <- c("vol", "dum")

#does vol granger cause dum
grangertest(y[,c("vol","dum")], order = 29)
#merde, y a une causalité

#does dum granger cause vol
grangertest(y[,c("dum", "vol")], order = 29)
#ouaich sa mère

#Variane decomposition
vd = fevd(SVAR, n.ahead = 20)
plot(vd)




#---------------------------------------------------------------------------------------------
#tarrif
y3 =cbind(Vdata$r_vol_h, Vdata$total_tariff)

y_lag3 = VARselect(y3, lag.max = 50)
y_list3 = list(y_lag3)

est.VAR3 = VAR(y3,p=21)
summary(est.VAR3)

#Structural breaks in the errors (stability)
stru3 = stability(est.VAR3, type ="OLS-CUSUM") #OLS-CUSUM or Rec-CUSUM
plot(stru3)


##create SVAR model
SVAR2 = SVAR(est.VAR3, estmethod = c("scoring", "direct"), Amat = NULL, Bmat = bmat)
summary(SVAR2)

IRF2 <- irf(SVAR2, response = "y1", impulse = "y2", 
            n.ahead = 40, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(IRF2)


#granger causality
#we want the smaller p-value => h0 test if there is no causality
colnames(y3)[1:2] <- c("vol", "tarrif")

#does vol granger cause tarrif
grangertest(y3[,c("vol","tarrif")], order = 29)
#mdr

#does tarrif granger cause vol
grangertest(y3[,c("tarrif", "vol")], order = 29)


#Variane decomposition
vd2 = fevd(SVAR2, n.ahead = 20)
plot(vd2)



#-----------------------------------------------------------------------------------------------


#trade
y4 =cbind(Vdata$r_vol_h, Vdata$total_trade)

y_lag4 = VARselect(y4, lag.max = 80)
y_list4 = list(y_lag4)

est.VAR4 = VAR(y4,p=21)
summary(est.VAR4)


##create SVAR model
SVAR4 = SVAR(est.VAR4, estmethod = c("scoring", "direct"), Amat = NULL, Bmat = bmat)
summary(SVAR4)

IRF4 <- irf(SVAR4, response = "y1", impulse = "y2", 
            n.ahead = 40, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(IRF4)


#granger causality
#we want the smaller p-value => h0 test if there is no causality
colnames(y4)[1:2] <- c("vol", "trade")

#does vol granger cause tarrif
grangertest(y4[,c("vol","trade")], order = 29)
 

#does tarrif granger cause vol
grangertest(y4[,c("trade", "vol")], order = 29)


#Variane decomposition
vd4 = fevd(SVAR4, n.ahead = 20)
plot(vd4)


#-----------------------------------------------------------------------------------------------
##sentiment analysis(N)

#find anger
tweetanger_alltime = dplyr::select(social_hourly,timestamp,anger)
#select time period
tweetanger = filter(tweetanger_alltime,
                    between(timestamp,
                            as.Date('2013-01-01'),
                            as.Date('2025-04-10')))

#find fear
tweetfear_alltime = dplyr::select(social_hourly,timestamp,fear)
#select time period
tweetfear = filter(tweetfear_alltime,
                   between(timestamp,
                           as.Date('2013-01-01'),
                           as.Date('2025-04-10')))

#find anticipation
anti_alltime = dplyr::select(social_hourly,timestamp,anticipation)
#select time period
anti = filter(anti_alltime,
              between(timestamp,
                      as.Date('2013-01-01'),
                      as.Date('2025-04-10')))


#find disgust
disgust_alltime = dplyr::select(social_hourly,timestamp,disgust)
#select time period
disgust = filter(disgust_alltime,
                 between(timestamp,
                         as.Date('2013-01-01'),
                         as.Date('2025-04-10')))


#find joy
joy_alltime = dplyr::select(social_hourly,timestamp,joy)
#select time period
joy = filter(joy_alltime,
             between(timestamp,
                     as.Date('2013-01-01'),
                     as.Date('2025-04-10')))

#find sadness
sadness_alltime = dplyr::select(social_hourly,timestamp,sadness)
#select time period
sadness = filter(sadness_alltime,
                 between(timestamp,
                         as.Date('2013-01-01'),
                         as.Date('2025-04-10')))

#find surprise
surprise_alltime = dplyr::select(social_hourly,timestamp,surprise)
#select time period
surprise = filter(surprise_alltime,
                  between(timestamp,
                          as.Date('2013-01-01'),
                          as.Date('2025-04-10')))

#find trust
trust_alltime = dplyr::select(social_hourly,timestamp,trust)
#select time period
trust = filter(trust_alltime,
               between(timestamp,
                       as.Date('2013-01-01'),
                       as.Date('2025-04-10')))

#find negative
negative_alltime = dplyr::select(social_hourly,timestamp,negative)
#select time period
negative = filter(negative_alltime,
                  between(timestamp,
                          as.Date('2013-01-01'),
                          as.Date('2025-04-10')))

#find positive
positive_alltime = dplyr::select(social_hourly,timestamp,positive)
#select time period
positive = filter(positive_alltime,
                  between(timestamp,
                          as.Date('2013-01-01'),
                          as.Date('2025-04-10')))

#find tot_posneg
tot_posneg_alltime = dplyr::select(social_hourly,timestamp,total_posneg)
#select time period
tot_posneg = filter(tot_posneg_alltime,
                    between(timestamp,
                            as.Date('2013-01-01'),
                            as.Date('2025-04-10')))

#find total_sentiment
tot_sent_alltime = dplyr::select(social_hourly,timestamp,total_sentiment)
#select time period
tot_sent = filter(tot_sent_alltime,
                  between(timestamp,
                          as.Date('2013-01-01'),
                          as.Date('2025-04-10')))




#merge our dependant and independant vars
Vdata2 = left_join(ASHR_volatility, tweetanger, by="timestamp")
Vdata2  = left_join(Vdata2 , tweetfear, by="timestamp")
Vdata2  = left_join(Vdata2 , anti, by="timestamp")
Vdata2  = left_join(Vdata2 , disgust, by="timestamp")
Vdata2  = left_join(Vdata2 , joy, by="timestamp")
Vdata2  = left_join(Vdata2 , sadness, by="timestamp")
Vdata2  = left_join(Vdata2 , surprise, by="timestamp")
Vdata2  = left_join(Vdata2 , trust, by="timestamp")
Vdata2  = left_join(Vdata2 , negative, by="timestamp")
Vdata2  = left_join(Vdata2 , positive, by="timestamp")
Vdata2  = left_join(Vdata2 , tot_posneg, by="timestamp")
Vdata2  = left_join(Vdata2 , tot_sent, by="timestamp")

#convert NA to zeroes
Vdata2 $anger[is.na(Vdata2 $anger)] = 0
Vdata2 $fear[is.na(Vdata2 $fear)] = 0
Vdata2 $anticipation[is.na(Vdata2 $anticipation)] = 0
Vdata2 $disgust[is.na(Vdata2 $disgust)] = 0
Vdata2 $joy[is.na(Vdata2 $joy)] = 0
Vdata2 $sadness[is.na(Vdata2 $sadness)] = 0
Vdata2 $surprise[is.na(Vdata2 $surprise)] = 0
Vdata2 $trust[is.na(Vdata2 $trust)] = 0
Vdata2 $negative[is.na(Vdata2 $negative)] = 0
Vdata2 $positive[is.na(Vdata2 $positive)] = 0
Vdata2 $total_posneg[is.na(Vdata2 $total_posneg)] = 0
Vdata2 $total_sentiment[is.na(Vdata2 $total_sentiment)] = 0

Vdata2 <- Vdata2 %>%
  filter(!is.na(r_vol_h))



#estimation 1 : Anger
y5 =cbind(Vdata2$r_vol_h, Vdata2$anger)

y_lag5 = VARselect(y5, lag.max = 80)
y_list5 = list(y_lag5)

est.VAR5 = VAR(y5,p=29)
summary(est.VAR5)


##create SVAR model
SVAR5 = SVAR(est.VAR5, estmethod = c("scoring", "direct"), Amat = NULL, Bmat = bmat)
summary(SVAR5)

IRF5 <- irf(SVAR5, response = "y1", impulse = "y2", 
            n.ahead = 40, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(IRF5)


#granger causality
#we want the smaller p-value => h0 test if there is no causality
colnames(y5)[1:2] <- c("vol", "anger")

#does vol granger cause anger
grangertest(y5[,c("vol","anger")], order = 29)


#does anger granger cause vol
grangertest(y5[,c("anger", "vol")], order = 29)


#Variane decomposition
vd5 = fevd(SVAR5, n.ahead = 20)
plot(vd5)



#estimation 2 : fear
y6 =cbind(Vdata2$r_vol_h, Vdata2$fear)

y_lag6 = VARselect(y6, lag.max = 80)
y_list6 = list(y_lag6)

est.VAR6 = VAR(y6,p=29)
summary(est.VAR6)


##create SVAR model
SVAR6 = SVAR(est.VAR6, estmethod = c("scoring", "direct"), Amat = NULL, Bmat = bmat)
summary(SVAR6)

IRF6 <- irf(SVAR6, response = "y1", impulse = "y2", 
            n.ahead = 40, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(IRF6)


#granger causality
#we want the smaller p-value => h0 test if there is no causality
colnames(y6)[1:2] <- c("vol", "fear")

#does vol granger cause fear
grangertest(y6[,c("vol","fear")], order = 29)


#does fear granger cause vol
grangertest(y6[,c("fear", "vol")], order = 29)


#Variane decomposition
vd6 = fevd(SVAR6, n.ahead = 20)
plot(vd6)


#estimation 3 : anticipation
y7 =cbind(Vdata2$r_vol_h, Vdata2$anticipation)

y_lag7 = VARselect(y7, lag.max = 80)
y_list7 = list(y_lag7)

est.VAR7 = VAR(y7,p=29)
summary(est.VAR7)


##create SVAR model
SVAR7 = SVAR(est.VAR7, estmethod = c("scoring", "direct"), Amat = NULL, Bmat = bmat)
summary(SVAR7)

IRF7 <- irf(SVAR7, response = "y1", impulse = "y2", 
            n.ahead = 40, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(IRF7)


#granger causality
#we want the smaller p-value => h0 test if there is no causality
colnames(y7)[1:2] <- c("vol", "anticipation")

#does vol granger cause anticipation
grangertest(y7[,c("vol","anticipation")], order = 29)


#does anticipation granger cause vol
grangertest(y7[,c("anticipation", "vol")], order = 29)


#Variane decomposition
vd7 = fevd(SVAR7, n.ahead = 20)
plot(vd7)


#estimation 4 : disgust
y9 =cbind(Vdata2$r_vol_h, Vdata2$disgust)

y_lag9 = VARselect(y9, lag.max = 80)
y_list9 = list(y_lag9)

est.VAR9 = VAR(y9,p=29)
summary(est.VAR9)


##create SVAR model
SVAR9 = SVAR(est.VAR9, estmethod = c("scoring", "direct"), Amat = NULL, Bmat = bmat)
summary(SVAR9)

IRF9 <- irf(SVAR9, response = "y1", impulse = "y2", 
            n.ahead = 40, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(IRF9)


#granger causality
#we want the smaller p-value => h0 test if there is no causality
colnames(y9)[1:2] <- c("vol", "disgust")

#does vol granger cause disgust
grangertest(y9[,c("vol","disgust")], order = 29)


#does disgust granger cause vol
grangertest(y9[,c("disgust", "vol")], order = 29)


#Variane decomposition
vd9 = fevd(SVAR9, n.ahead = 20)
plot(vd9)


#estimation 5 : joy
y10 =cbind(Vdata2$r_vol_h, Vdata2$joy)

y_lag10 = VARselect(y10, lag.max = 80)
y_list10 = list(y_lag10)

est.VAR10 = VAR(y10,p=29)
summary(est.VAR10)


##create SVAR model
SVAR10 = SVAR(est.VAR10, estmethod = c("scoring", "direct"), Amat = NULL, Bmat = bmat)
summary(SVAR10)

IRF10 <- irf(SVAR10, response = "y1", impulse = "y2", 
             n.ahead = 50, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(IRF10)


#granger causality
#we want the smaller p-value => h0 test if there is no causality
colnames(y10)[1:2] <- c("vol", "joy")

#does vol granger cause joy
grangertest(y10[,c("vol","joy")], order = 29)


#does joy granger cause vol
grangertest(y10[,c("joy", "vol")], order = 29)


#Variane decomposition
vd10 = fevd(SVAR10, n.ahead = 20)
plot(vd10)


#estimation 6 : sadness
y11 =cbind(Vdata2$r_vol_h, Vdata2$sadness)

y_lag11 = VARselect(y11, lag.max = 80)
y_list11 = list(y_lag11)

est.VAR11 = VAR(y11,p=29)
summary(est.VAR11)


##create SVAR model
SVAR11 = SVAR(est.VAR11, estmethod = c("scoring", "direct"), Amat = NULL, Bmat = bmat)
summary(SVAR11)

IRF11 <- irf(SVAR11, response = "y1", impulse = "y2", 
             n.ahead = 40, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(IRF11)


#granger causality
#we want the smaller p-value => h0 test if there is no causality
colnames(y11)[1:2] <- c("vol", "sadness")

#does vol granger cause sadness
grangertest(y11[,c("vol","sadness")], order = 29)


#does sadness granger cause vol
grangertest(y11[,c("sadness", "vol")], order = 29)


#Variane decomposition
vd11 = fevd(SVAR11, n.ahead = 20)
plot(vd11)


#estimation 7 : surprise
y12 =cbind(Vdata2$r_vol_h, Vdata2$surprise)

y_lag12 = VARselect(y12, lag.max = 80)
y_list12 = list(y_lag12)

est.VAR12 = VAR(y12,p=29)
summary(est.VAR12)


##create SVAR model
SVAR12 = SVAR(est.VAR12, estmethod = c("scoring", "direct"), Amat = NULL, Bmat = bmat)
summary(SVAR12)

IRF12 <- irf(SVAR12, response = "y1", impulse = "y2", 
             n.ahead = 40, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(IRF12)


#granger causality
#we want the smaller p-value => h0 test if there is no causality
colnames(y12)[1:2] <- c("vol", "surprise")

#does vol granger cause surprise
grangertest(y12[,c("vol","surprise")], order = 29)


#does surprise granger cause vol
grangertest(y12[,c("surprise", "vol")], order = 29)


#Variane decomposition
vd12 = fevd(SVAR12, n.ahead = 20)
plot(vd12)


#estimation 8 : trust
y13 =cbind(Vdata2$r_vol_h, Vdata2$trust)

y_lag13 = VARselect(y13, lag.max = 80)
y_list13 = list(y_lag13)

est.VAR13 = VAR(y13,p=29)
summary(est.VAR13)


##create SVAR model
SVAR13 = SVAR(est.VAR13, estmethod = c("scoring", "direct"), Amat = NULL, Bmat = bmat)
summary(SVAR13)

IRF13 <- irf(SVAR13, response = "y1", impulse = "y2", 
             n.ahead = 40, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(IRF13)


#granger causality
#we want the smaller p-value => h0 test if there is no causality
colnames(y13)[1:2] <- c("vol", "trust")

#does vol granger cause trust
grangertest(y13[,c("vol","trust")], order = 29)


#does trust granger cause vol
grangertest(y13[,c("trust", "vol")], order = 29)


#Variane decomposition
vd13 = fevd(SVAR13, n.ahead = 20)
plot(vd13)


#estimation 9 : negative
y14 =cbind(Vdata2$r_vol_h, Vdata2$negative)

y_lag14 = VARselect(y14, lag.max = 80)
y_list14 = list(y_lag14)

est.VAR14 = VAR(y14,p=29)
summary(est.VAR14)


##create SVAR model
SVAR14 = SVAR(est.VAR14, estmethod = c("scoring", "direct"), Amat = NULL, Bmat = bmat)
summary(SVAR14)

IRF14 <- irf(SVAR14, response = "y1", impulse = "y2", 
             n.ahead = 40, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(IRF14)


#granger causality
#we want the smaller p-value => h0 test if there is no causality
colnames(y14)[1:2] <- c("vol", "negative")

#does vol granger cause negative
grangertest(y14[,c("vol","negative")], order = 29)


#does negative granger cause vol
grangertest(y14[,c("negative", "vol")], order = 29)


#Variane decomposition
vd14 = fevd(SVAR14, n.ahead = 20)
plot(vd14)


#estimation 10 : positive
y15 =cbind(Vdata2$r_vol_h, Vdata2$positive)

y_lag15 = VARselect(y15, lag.max = 80)
y_list15 = list(y_lag15)

est.VAR15 = VAR(y15,p=29)
summary(est.VAR15)


##create SVAR model
SVAR15 = SVAR(est.VAR15, estmethod = c("scoring", "direct"), Amat = NULL, Bmat = bmat)
summary(SVAR15)

IRF15 <- irf(SVAR15, response = "y1", impulse = "y2", 
             n.ahead = 40, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(IRF15)


#granger causality
#we want the smaller p-value => h0 test if there is no causality
colnames(y15)[1:2] <- c("vol", "positive")

#does vol granger cause positive
grangertest(y15[,c("vol","positive")], order = 29)


#does positive granger cause vol
grangertest(y15[,c("positive", "vol")], order = 29)


#Variane decomposition
vd15 = fevd(SVAR15, n.ahead = 20)
plot(vd15)


#estimation 11 : total_posneg
y16 =cbind(Vdata2$r_vol_h, Vdata2$total_posneg)

y_lag16 = VARselect(y16, lag.max = 80)
y_list16 = list(y_lag16)

est.VAR16 = VAR(y16,p=29)
summary(est.VAR16)


##create SVAR model
SVAR16 = SVAR(est.VAR16, estmethod = c("scoring", "direct"), Amat = NULL, Bmat = bmat)
summary(SVAR16)

IRF16 <- irf(SVAR16, response = "y1", impulse = "y2", 
             n.ahead = 40, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(IRF16)


#granger causality
#we want the smaller p-value => h0 test if there is no causality
colnames(y16)[1:2] <- c("vol", "total_posneg")

#does vol granger cause total_posneg
grangertest(y16[,c("vol","total_posneg")], order = 29)


#does total_posneg granger cause vol
grangertest(y16[,c("total_posneg", "vol")], order = 29)


#Variane decomposition
vd16 = fevd(SVAR16, n.ahead = 20)
plot(vd16)



#estimation 12 : total_sentiment
y17 =cbind(Vdata2$r_vol_h, Vdata2$total_sentiment)

y_lag17 = VARselect(y17, lag.max = 80)
y_list17 = list(y_lag17)

est.VAR17 = VAR(y17,p=29)
summary(est.VAR17)


##create SVAR model
SVAR17 = SVAR(est.VAR17, estmethod = c("scoring", "direct"), Amat = NULL, Bmat = bmat)
summary(SVAR17)

IRF17 <- irf(SVAR17, response = "y1", impulse = "y2", 
             n.ahead = 40, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(IRF17)


#granger causality
#we want the smaller p-value => h0 test if there is no causality
colnames(y17)[1:2] <- c("vol", "total_sentiment")

#does vol granger cause total_sentiment
grangertest(y17[,c("vol","total_sentiment")], order = 29)
#mdr 

#does total_sentiment granger cause vol
grangertest(y17[,c("total_sentiment", "vol")], order = 29)


#Variane decomposition
vd17 = fevd(SVAR17, n.ahead = 20)
plot(vd17)

#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
##sentiment analysis(prop)

#find anger
tweetprop_anger_alltime = dplyr::select(social_hourly,timestamp,prop_anger)
#select time period
tweetprop_anger = filter(tweetprop_anger_alltime,
                         between(timestamp,
                                 as.Date('2013-01-01'),
                                 as.Date('2025-04-10')))

#find fear
tweetprop_fear_alltime = dplyr::select(social_hourly,timestamp,prop_fear)
#select time period
tweetprop_fear = filter(tweetprop_fear_alltime,
                        between(timestamp,
                                as.Date('2013-01-01'),
                                as.Date('2025-04-10')))

#find anticipation
prop_anticipation_alltime = dplyr::select(social_hourly,timestamp,prop_anticipation)
#select time period
prop_anticipation = filter(prop_anticipation_alltime,
                           between(timestamp,
                                   as.Date('2013-01-01'),
                                   as.Date('2025-04-10')))


#find disgust
prop_disgust_alltime = dplyr::select(social_hourly,timestamp,prop_disgust)
#select time period
prop_disgust = filter(prop_disgust_alltime,
                      between(timestamp,
                              as.Date('2013-01-01'),
                              as.Date('2025-04-10')))


#find joy
prop_joy_alltime = dplyr::select(social_hourly,timestamp,prop_joy)
#select time period
prop_joy = filter(prop_joy_alltime,
                  between(timestamp,
                          as.Date('2013-01-01'),
                          as.Date('2025-04-10')))

#find sadness
prop_sadness_alltime = dplyr::select(social_hourly,timestamp,prop_sadness)
#select time period
prop_sadness = filter(prop_sadness_alltime,
                      between(timestamp,
                              as.Date('2013-01-01'),
                              as.Date('2025-04-10')))

#find surprise
prop_surprise_alltime = dplyr::select(social_hourly,timestamp,prop_surprise)
#select time period
prop_surprise = filter(prop_surprise_alltime,
                       between(timestamp,
                               as.Date('2013-01-01'),
                               as.Date('2025-04-10')))

#find trust
prop_trust_alltime = dplyr::select(social_hourly,timestamp,prop_trust)
#select time period
prop_trust = filter(prop_trust_alltime,
                    between(timestamp,
                            as.Date('2013-01-01'),
                            as.Date('2025-04-10')))

#find negative
prop_negative_alltime = dplyr::select(social_hourly,timestamp,prop_negative)
#select time period
prop_negative = filter(prop_negative_alltime,
                       between(timestamp,
                               as.Date('2013-01-01'),
                               as.Date('2025-04-10')))

#find positive
prop_positive_alltime = dplyr::select(social_hourly,timestamp,prop_positive)
#select time period
prop_positive = filter(prop_positive_alltime,
                       between(timestamp,
                               as.Date('2013-01-01'),
                               as.Date('2025-04-10')))





#merge our dependant and independant vars
Vdata3 = left_join(SPY_volatility, tweetprop_anger, by="timestamp")
Vdata3  = left_join(Vdata3 , tweetprop_fear, by="timestamp")
Vdata3  = left_join(Vdata3 , prop_anticipation, by="timestamp")
Vdata3  = left_join(Vdata3 , prop_disgust, by="timestamp")
Vdata3  = left_join(Vdata3 , prop_joy, by="timestamp")
Vdata3  = left_join(Vdata3 , prop_sadness, by="timestamp")
Vdata3  = left_join(Vdata3 , prop_surprise, by="timestamp")
Vdata3  = left_join(Vdata3 , prop_trust, by="timestamp")
Vdata3  = left_join(Vdata3 , prop_negative, by="timestamp")
Vdata3  = left_join(Vdata3 , prop_positive, by="timestamp")


#convert NA to zeroes
Vdata3 $prop_anger[is.na(Vdata3 $prop_anger)] = 0
Vdata3 $prop_fear[is.na(Vdata3 $prop_fear)] = 0
Vdata3 $prop_anticipation[is.na(Vdata3 $prop_anticipation)] = 0
Vdata3 $prop_disgust[is.na(Vdata3 $prop_disgust)] = 0
Vdata3 $prop_joy[is.na(Vdata3 $prop_joy)] = 0
Vdata3 $prop_sadness[is.na(Vdata3 $prop_sadness)] = 0
Vdata3 $prop_surprise[is.na(Vdata3 $prop_surprise)] = 0
Vdata3 $prop_trust[is.na(Vdata3 $prop_trust)] = 0
Vdata3 $prop_negative[is.na(Vdata3 $prop_negative)] = 0
Vdata3 $prop_positive[is.na(Vdata3 $prop_positive)] = 0





#estimation 1 : prop_anger
y18 =cbind(Vdata3$r_vol_h, Vdata3$prop_anger)

y_lag18 = VARselect(y18, lag.max = 80)
y_list18 = list(y_lag18)

est.VAR18 = VAR(y18,p=29)
summary(est.VAR18)


##create SVAR model
SVAR18 = SVAR(est.VAR18, estmethod = c("scoring", "direct"), Amat = NULL, Bmat = bmat)
summary(SVAR18)

IRF18 <- irf(SVAR18, response = "y1", impulse = "y2", 
             n.ahead = 40, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(IRF18)


#granger causality
#we want the smaller p-value => h0 test if there is no causality
colnames(y18)[1:2] <- c("vol", "prop_anger")

#does vol granger cause prop_anger
grangertest(y18[,c("vol","prop_anger")], order = 29)


#does prop_anger granger cause vol
grangertest(y18[,c("prop_anger", "vol")], order = 29)


#Variane decomposition
vd18 = fevd(SVAR18, n.ahead = 20)
plot(vd18)



#estimation 2 : prop_fear
y19 =cbind(Vdata3$r_vol_h, Vdata3$prop_fear)

y_lag19 = VARselect(y19, lag.max = 80)
y_list19 = list(y_lag19)

est.VAR19 = VAR(y19,p=29)
summary(est.VAR19)


##create SVAR model
SVAR19 = SVAR(est.VAR19, estmethod = c("scoring", "direct"), Amat = NULL, Bmat = bmat)
summary(SVAR19)

IRF19 <- irf(SVAR19, response = "y1", impulse = "y2", 
             n.ahead = 40, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(IRF19)


#granger causality
#we want the smaller p-value => h0 test if there is no causality
colnames(y19)[1:2] <- c("vol", "prop_fear")

#does vol granger cause prop_fear
grangertest(y19[,c("vol","prop_fear")], order = 29)


#does prop_fear granger cause vol
grangertest(y19[,c("prop_fear", "vol")], order = 29)


#Variane decomposition
vd19 = fevd(SVAR19, n.ahead = 20)
plot(vd19)


#estimation 3 : prop_anticipation
y20 =cbind(Vdata3$r_vol_h, Vdata3$prop_anticipation)

y_lag20 = VARselect(y20, lag.max = 80)
y_list20 = list(y_lag20)

est.VAR20 = VAR(y20,p=29)
summary(est.VAR20)


##create SVAR model
SVAR20 = SVAR(est.VAR20, estmethod = c("scoring", "direct"), Amat = NULL, Bmat = bmat)
summary(SVAR20)

IRF20 <- irf(SVAR20, response = "y1", impulse = "y2", 
             n.ahead = 40, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(IRF20)


#granger causality
#we want the smaller p-value => h0 test if there is no causality
colnames(y20)[1:2] <- c("vol", "prop_anticipation")

#does vol granger cause prop_anticipation
grangertest(y20[,c("vol","prop_anticipation")], order = 29)


#does prop_anticipation granger cause vol
grangertest(y20[,c("prop_anticipation", "vol")], order = 29)


#Variane decomposition
vd20 = fevd(SVAR20, n.ahead = 20)
plot(vd20)


#estimation 4 : prop_disgust
y21 =cbind(Vdata3$r_vol_h, Vdata3$prop_disgust)

y_lag21 = VARselect(y21, lag.max = 80)
y_list21 = list(y_lag21)

est.VAR21 = VAR(y21,p=29)
summary(est.VAR21)


##create SVAR model
SVAR21 = SVAR(est.VAR21, estmethod = c("scoring", "direct"), Amat = NULL, Bmat = bmat)
summary(SVAR21)

IRF21 <- irf(SVAR21, response = "y1", impulse = "y2", 
             n.ahead = 40, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(IRF21)


#granger causality
#we want the smaller p-value => h0 test if there is no causality
colnames(y21)[1:2] <- c("vol", "prop_disgust")

#does vol granger cause prop_disgust
grangertest(y21[,c("vol","prop_disgust")], order = 29)


#does prop_disgust granger cause vol
grangertest(y21[,c("prop_disgust", "vol")], order = 29)


#Variane decomposition
vd21 = fevd(SVAR21, n.ahead = 20)
plot(vd21)


#estimation 5 : prop_joy
y27 =cbind(Vdata3$r_vol_h, Vdata3$prop_joy)

y_lag27 = VARselect(y27, lag.max = 80)
y_list27 = list(y_lag27)

est.VAR27 = VAR(y27,p=29)
summary(est.VAR27)


##create SVAR model
SVAR27 = SVAR(est.VAR27, estmethod = c("scoring", "direct"), Amat = NULL, Bmat = bmat)
summary(SVAR27)

IRF27 <- irf(SVAR27, response = "y1", impulse = "y2", 
             n.ahead = 50, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(IRF27)


#granger causality
#we want the smaller p-value => h0 test if there is no causality
colnames(y27)[1:2] <- c("vol", "prop_joy")

#does vol granger cause prop_joy
grangertest(y27[,c("vol","prop_joy")], order = 29)


#does prop_joy granger cause vol
grangertest(y27[,c("prop_joy", "vol")], order = 29)


#Variane decomposition
vd27 = fevd(SVAR27, n.ahead = 20)
plot(vd27)


#estimation 6 : prop_sadness
y22 =cbind(Vdata3$r_vol_h, Vdata3$prop_sadness)

y_lag22 = VARselect(y22, lag.max = 80)
y_list22 = list(y_lag22)

est.VAR22 = VAR(y22,p=29)
summary(est.VAR22)


##create SVAR model
SVAR22 = SVAR(est.VAR22, estmethod = c("scoring", "direct"), Amat = NULL, Bmat = bmat)
summary(SVAR22)

IRF22 <- irf(SVAR22, response = "y1", impulse = "y2", 
             n.ahead = 40, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(IRF22)


#granger causality
#we want the smaller p-value => h0 test if there is no causality
colnames(y22)[1:2] <- c("vol", "prop_sadness")

#does vol granger cause prop_sadness
grangertest(y22[,c("vol","prop_sadness")], order = 29)


#does prop_sadness granger cause vol
grangertest(y22[,c("prop_sadness", "vol")], order = 29)


#Variane decomposition
vd22 = fevd(SVAR22, n.ahead = 20)
plot(vd22)


#estimation 7 : prop_surprise
y23 =cbind(Vdata3$r_vol_h, Vdata3$prop_surprise)

y_lag23 = VARselect(y23, lag.max = 80)
y_list23 = list(y_lag23)

est.VAR23 = VAR(y23,p=29)
summary(est.VAR23)


##create SVAR model
SVAR23 = SVAR(est.VAR23, estmethod = c("scoring", "direct"), Amat = NULL, Bmat = bmat)
summary(SVAR23)

IRF23 <- irf(SVAR23, response = "y1", impulse = "y2", 
             n.ahead = 40, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(IRF23)


#granger causality
#we want the smaller p-value => h0 test if there is no causality
colnames(y23)[1:2] <- c("vol", "prop_surprise")

#does vol granger cause prop_surprise
grangertest(y23[,c("vol","prop_surprise")], order = 29)


#does prop_surprise granger cause vol
grangertest(y23[,c("prop_surprise", "vol")], order = 29)


#Variane decomposition
vd23 = fevd(SVAR23, n.ahead = 20)
plot(vd23)


#estimation 8 : prop_trust
y24 =cbind(Vdata3$r_vol_h, Vdata3$prop_trust)

y_lag24 = VARselect(y24, lag.max = 80)
y_list24 = list(y_lag24)

est.VAR24 = VAR(y24,p=29)
summary(est.VAR24)


##create SVAR model
SVAR24 = SVAR(est.VAR24, estmethod = c("scoring", "direct"), Amat = NULL, Bmat = bmat)
summary(SVAR24)

IRF24 <- irf(SVAR24, response = "y1", impulse = "y2", 
             n.ahead = 40, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(IRF24)


#granger causality
#we want the smaller p-value => h0 test if there is no causality
colnames(y24)[1:2] <- c("vol", "prop_trust")

#does vol granger cause prop_trust
grangertest(y24[,c("vol","prop_trust")], order = 29)


#does prop_trust granger cause vol
grangertest(y24[,c("prop_trust", "vol")], order = 29)


#Variane decomposition
vd24 = fevd(SVAR24, n.ahead = 20)
plot(vd24)


#estimation 9 : prop_negative
y25 =cbind(Vdata3$r_vol_h, Vdata3$prop_negative)

y_lag25 = VARselect(y25, lag.max = 80)
y_list25 = list(y_lag25)

est.VAR25 = VAR(y25,p=29)
summary(est.VAR25)


##create SVAR model
SVAR25 = SVAR(est.VAR25, estmethod = c("scoring", "direct"), Amat = NULL, Bmat = bmat)
summary(SVAR25)

IRF25 <- irf(SVAR25, response = "y1", impulse = "y2", 
             n.ahead = 40, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(IRF25)


#granger causality
#we want the smaller p-value => h0 test if there is no causality
colnames(y25)[1:2] <- c("vol", "prop_negative")

#does vol granger cause prop_negative
grangertest(y25[,c("vol","prop_negative")], order = 29)


#does prop_negative granger cause vol
grangertest(y25[,c("prop_negative", "vol")], order = 29)


#Variane decomposition
vd25 = fevd(SVAR25, n.ahead = 20)
plot(vd25)


#estimation 10 : prop_positive
y26 =cbind(Vdata3$r_vol_h, Vdata3$prop_positive)

y_lag26 = VARselect(y26, lag.max = 80)
y_list26 = list(y_lag26)

est.VAR26 = VAR(y26,p=29)
summary(est.VAR26)


##create SVAR model
SVAR26 = SVAR(est.VAR26, estmethod = c("scoring", "direct"), Amat = NULL, Bmat = bmat)
summary(SVAR26)

IRF26 <- irf(SVAR26, response = "y1", impulse = "y2", 
             n.ahead = 40, ortho = TRUE, boot = TRUE)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(IRF26)


#granger causality
#we want the smaller p-value => h0 test if there is no causality
colnames(y26)[1:2] <- c("vol", "prop_positive")

#does vol granger cause prop_positive
grangertest(y26[,c("vol","prop_positive")], order = 29)


#does prop_positive granger cause vol
grangertest(y26[,c("prop_positive", "vol")], order = 29)


#Variane decomposition
vd26 = fevd(SVAR26, n.ahead = 20)
plot(vd26)
