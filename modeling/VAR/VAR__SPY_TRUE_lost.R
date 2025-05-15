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
require(sandwich) #regression errors
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


# Data 


## Load Base Data

# 1. Load Political Social Media

#contains posts from Twitter & TruthSocial
social <- read.csv(here("data/mothership", "social.csv"))

social_hourly <- read.csv(here("data/mothership", "socialhourly.csv"))


# 2. Load Financial

#S&P500
SPY <- read.csv(here("data/mothership", "SPY.csv"))

#STOXX50
VGK <- read.csv(here("data/mothership", "VGK.csv"))

#CSI 300 (China)
ASHR <- read.csv(here("data/mothership", "ASHR.CSV"))




#make posixct
SPY$timestamp = as.POSIXct(SPY$timestamp,format = "%Y-%m-%d %H:%M:%S")
VGK$timestamp = as.POSIXct(VGK$timestamp,format = "%Y-%m-%d %H:%M:%S")
ASHR$timestamp = as.POSIXct(ASHR$timestamp,format = "%Y-%m-%d %H:%M:%S")
social$timestamp = as.POSIXct(social$timestamp,format = "%Y-%m-%d %H:%M:%S")
social_hourly$timestamp = as.POSIXct(social_hourly$timestamp,format = "%Y-%m-%d %H:%M:%S")
social_hourly$adjusted_time = as.POSIXct(social_hourly$adjusted_time,format = "%Y-%m-%d %H:%M:%S")

#select timeframe 
SPY = filter(SPY,between(timestamp, as.Date('2014-01-01'), as.Date('2025-05-07')))
VGK = filter(VGK,between(timestamp, as.Date('2014-01-01'), as.Date('2025-05-07')))
ASHR = filter(ASHR,between(timestamp, as.Date('2014-01-01'), as.Date('2025-05-07')))
social = filter(social,between(timestamp, as.Date('2014-01-01'), as.Date('2025-05-07')))
social_hourly = filter(social_hourly,between(timestamp, as.Date('2014-01-01'), as.Date('2025-05-07')))
#SPY = filter(SPY,between(timestamp, as.Date('2018-01-01'), as.Date('2025-05-07')))
#VGK = filter(VGK,between(timestamp, as.Date('2018-01-01'), as.Date('2025-05-07')))
#ASHR = filter(ASHR,between(timestamp, as.Date('2018-01-01'), as.Date('2025-05-07')))
#social = filter(social,between(timestamp, as.Date('2018-01-01'), as.Date('2025-05-07')))
#social_hourly = filter(social_hourly,between(timestamp, as.Date('2018-01-01'), as.Date('2025-05-07')))




## Volatility

#find hourly volatility
SPY_volatility = dplyr::select(SPY,timestamp,r_vol_h)

#aggregating per hour
SPY_volatility = SPY_volatility %>%
  mutate(timestamp = floor_date(timestamp, unit = "hour")) %>%
  distinct(timestamp, .keep_all = TRUE) 

#plot
hvol_plotter(SPY_volatility,breaks="3 month", 
             title="Realised Volatility - SPY")




#find hourly volatility
VGK_volatility = dplyr::select(VGK,timestamp,r_vol_h)

#aggregating per hour
VGK_volatility = VGK_volatility %>%
  mutate(timestamp = floor_date(timestamp, unit = "hour")) %>%
  distinct(timestamp, .keep_all = TRUE) 



#find hourly volatility
ASHR_volatility = dplyr::select(ASHR,timestamp,r_vol_h)

#aggregating per hour
ASHR_volatility = ASHR_volatility %>%
  mutate(timestamp = floor_date(timestamp, unit = "hour")) %>%
  distinct(timestamp, .keep_all = TRUE) 







#Find tweet variables

## Number of Posts 
#find count
tweetcount = dplyr::select(social_hourly,timestamp,adjusted_time,N)

#for taking count of closed market hours
tweetcount2 <- tweetcount %>%
  group_by(adjusted_time) %>%
  summarise(N = sum(N)) 

#plot
ggplot(tweetcount, aes(x = timestamp, y = N)) +
  geom_point(color = "#253494", size = 1) +
  scale_x_datetime(date_labels = "%b %Y", date_breaks = "3 month") +
  labs(title = "Trump Social Media Count",
       x = NULL,
       y = "number of tweets/truths") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", hjust = 0.5))




## Dummy for Social Media Post
#find dummy
tweetdummy = dplyr::select(social_hourly,timestamp,adjusted_time,dummy)

#for taking count of closed market hours
tweetdummy2 <- tweetdummy %>%
  group_by(adjusted_time) %>%
  summarise(dummy = sum(dummy)) 
#peculiar interpretation for dummy: if dummy>1 it means that there were x
#out-hours which had tweets in them



## Number of Tweets Mentioning Tariffs 
#find count
tariff = dplyr::select(social_hourly,timestamp,adjusted_time,total_tariff)

#for taking count of closed market hours
tariff2 <- tariff %>%
  group_by(adjusted_time) %>%
  summarise(total_tariff = sum(total_tariff)) 



## Number of Tweets Mentioning Trade 
#find count
trade = dplyr::select(social_hourly,timestamp,adjusted_time,total_trade)

#for taking count of closed market hours
trade2 <- trade %>%
  group_by(adjusted_time) %>%
  summarise(total_trade = sum(total_trade)) 




## Number of Tweets Mentioning China 
#find count
china = dplyr::select(social_hourly,timestamp,adjusted_time,total_china)

#for taking count of closed market hours
china2 <- china %>%
  group_by(adjusted_time) %>%
  summarise(total_china = sum(total_china)) 



## Proportion of Positive 
#find count
positive = dplyr::select(social_hourly,timestamp,adjusted_time,prop_positive)

#how to count outside hours? since proportion?



## Proportion of Negative 
#find count
negative = dplyr::select(social_hourly,timestamp,adjusted_time,prop_negative)



## Merge
#merge our dependant and independant vars

#case 1: ignore tweets outside trading hours
Vdata = left_join(SPY_volatility, VGK_volatility, by="timestamp")
Vdata = left_join(Vdata, ASHR_volatility, by="timestamp")
Vdata = left_join(Vdata, dplyr::select(tweetdummy, -adjusted_time), by="timestamp")
Vdata = left_join(Vdata, dplyr::select(tweetcount, -adjusted_time), by="timestamp")
Vdata = left_join(Vdata, dplyr::select(tariff, -adjusted_time), by="timestamp")
Vdata = left_join(Vdata, dplyr::select(trade, -adjusted_time), by="timestamp")
Vdata = left_join(Vdata, dplyr::select(china, -adjusted_time), by="timestamp")
Vdata = left_join(Vdata, dplyr::select(positive, -adjusted_time), by="timestamp")
Vdata = left_join(Vdata, dplyr::select(negative, -adjusted_time), by="timestamp")

rm(Vdata)
#case 2: push tweets made outside market hours to the next open hour
Vdata = left_join(SPY_volatility, VGK_volatility, by="timestamp")
Vdata = left_join(Vdata, ASHR_volatility, by="timestamp")
Vdata <- Vdata %>%
  left_join(tweetdummy2, by = c("timestamp" = "adjusted_time")) 
Vdata <- Vdata %>%
  left_join(tweetcount2, by = c("timestamp" = "adjusted_time")) 
Vdata <- Vdata %>%
  left_join(tariff2, by = c("timestamp" = "adjusted_time")) 
Vdata <- Vdata %>%
  left_join(trade2, by = c("timestamp" = "adjusted_time")) 
Vdata <- Vdata %>%
  left_join(china2, by = c("timestamp" = "adjusted_time")) 

#rename volatility columns
names(Vdata)[2] <- "SPY_vol"
names(Vdata)[3] <- "VGK_vol"
names(Vdata)[4] <- "ASHR_vol"

#convert NA to zeroes
Vdata$N[is.na(Vdata$N)] = 0
Vdata$dummy[is.na(Vdata$dummy)] = 0
Vdata$total_tariff[is.na(Vdata$total_tariff)] = 0
Vdata$total_trade[is.na(Vdata$total_trade)] = 0
Vdata$total_china[is.na(Vdata$total_china)] = 0
Vdata$prop_positive[is.na(Vdata$prop_positive)] = 0
Vdata$prop_negative[is.na(Vdata$prop_negative)] = 0




#oooooooooooooooooooooooooooooorrrrrrrrrrrrrrrrrrrrrrrrr

#load final dataset
source(here("helperfunctions/full_data.R"))

#select timeframe 
Vdata = filter(data,between(timestamp, as.Date('2014-01-01'), as.Date('2025-05-07')))




#-----------------------------------------------------------------------------------------
#Nombre of lag

#series
pro_v = ts(Vdata$SPY_vol)
pro_t_dummy = ts(Vdata$dummy)

autoplot(pro_v)
autoplot(pro_t_dummy)


##with dummy
y = cbind(Vdata$dummy, Vdata$SPY_vol)

y_lag = VARselect(y, lag.max = 70)
y_list = list(y_lag)


##AIC
resultats1 <- lapply(y_list, function(x) x$criteria["AIC(n)", ])

resultats1 = as.data.frame(resultats1)

resultats1 = resultats1 %>%
  rename(name = 1) %>%
  mutate(
    n = c(1:length(name))
  )

ggplot (resultats1, aes(x=n, y= name)) +
  geom_line(color = "steelblue") +
  labs(title = "hourly Returns AIC",x="dummy" , y = "AIC") +
  theme_minimal()


##HQ
resultats2 <- lapply(y_list, function(x) x$criteria["HQ(n)", ])

resultats2 = as.data.frame(resultats2)

resultats2 = resultats2 %>%
  rename(name = 1) %>%
  mutate(
    n = c(1:length(name))
  )

ggplot (resultats2, aes(x=n, y= name)) +
  geom_line(color = "steelblue") +
  labs(title = "hourly Returns HQ",x="dummy" , y = "HQ") +
  theme_minimal()



##SC
resultats3 <- lapply(y_list, function(x) x$criteria["SC(n)", ])

resultats3 = as.data.frame(resultats3)

resultats3 = resultats3 %>%
  rename(name = 1) %>%
  mutate(
    n = c(1:length(name))
  )

ggplot (resultats3, aes(x=n, y= name)) +
  geom_line(color = "steelblue") +
  labs(title = "hourly Returns SC",x="dum" , y = "SC") +
  theme_minimal()


##FPE
resultats4 <- lapply(y_list, function(x) x$criteria["FPE(n)", ])

resultats4 = as.data.frame(resultats4)

resultats4 = resultats4 %>%
  rename(name = 1) %>%
  mutate(
    n = c(1:length(name))
  )

ggplot (resultats4, aes(x=n, y= name)) +
  geom_line(color = "steelblue") +
  labs(title = "hourly Returns FPE",x="dum" , y = "FPE") +
  theme_minimal()








#-----------------------------------------------------------------------------------------------
#bunch of test 

###acf/pacf
acf(Vdata$SPY_vol)
pacf(Vdata$SPY_vol)

acf(Vdata$dummy)
pacf(Vdata$dummy)

####adf test (for stationarity of the outcome data) 
####p-value test H0, the probability that the process is not stationary
adf.test(Vdata$SPY_vol, k = 6)
adf.test(Vdata$dummy, k = 6)

##serial correlation
serial = serial.test(est.VAR, lags.pt = 20, type = "PT.asymptotic")
serial

#y1 is vol ; y2 is tweet (dummy or N)
plot(serial, names = "y1")
plot(serial, names = "dum")
###H0 test if there is no serial correlation
###we observe serial correlation


#heteroscedasticity
hete = arch.test(est.VAR, lags.multi = 80, multivariate.only = TRUE)
hete
##again, h0 test if there is no heteroscedasticity
## there is hetero


#Distribution of residuals
norm = normality.test(est.VAR, multivariate.only = TRUE)
norm
##first test tests the dirtribution
##second one tests the skewness
##third one test kurtosis
##all h0 test if there is not...
###it seems to fail every test


#Structural breaks in the errors (stability)
stru2 = stability(est.VAR, type ="OLS-CUSUM") #OLS-CUSUM or Rec-CUSUM
plot(stru2)
#--------------------------------------------------------------------------------------------





#---------------------------------------------------------------------------------------------
y = cbind(Vdata$dummy, Vdata$SPY_vol)
colnames(y)[1:2] <- c("dum", "vol")
est.VAR <- VAR(y,p=17)
summary(est.VAR)
Omega <- var(residuals(est.VAR))


#make the B matrix
loss <- function(param){
  #Define the restriction
  B <- matrix(c(param[1], param[2], 0, param[3]), ncol = 2)
  
  #Make BB' approximatively equal to omega
  X <- Omega - B %*% t(B)
  
  #loss function
  loss <- sum(X^2)
  return(loss)
}

res.opt <- optim(c(1, 0, 1), loss, method = "BFGS")
B.hat <- matrix(c(res.opt$par[1], res.opt$par[2], 0, res.opt$par[3]), ncol = 2)

print(cbind(Omega,B.hat %*% t(B.hat)))


#get back the coefficient of est.VAR
phi <- Acoef(est.VAR)
PHI = make.PHI(phi)

#take the constant
constant <- sapply(est.VAR$varresult, function(eq) coef(eq)["const"])
c=as.matrix(constant)

#Simulate the IRF
p <- length(phi)
n <- dim(phi[[1]])[1]
nb.sim <- 7*9
par(mfrow=c(2,2))

Y <- simul.VAR(c=c, Phi = phi, B = B.hat, nb.sim ,y0.star=rep(0, n*p),
                indic.IRF = 1, u.shock = c(1,0))
par(mfrow=c(1,1))  
par(plt=c(0.1, 0.9, 0.1, 0.9))
plot(Y[,2],type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")



#cumulative effect of a shock
plot(cumsum(Y[,2]),type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")




#granger causality
#we want the smaller p-value => h0 test if there is no causality
#does vol granger cause dum
grangertest(y[,c("vol","dum")], order = 17)

#does dum granger cause vol
grangertest(y[,c("dum", "vol")], order = 17)

#Variane decomposition
vd = fevd(Y, n.ahead = 20)
plot(vd)





#with N

y2 = cbind(Vdata$N, Vdata$SPY_vol)
colnames(y2)[1:2] <- c("N", "vol")
est.VAR2 <- VAR(y2,p=17)
summary(est.VAR2)
Omega2 <- var(residuals(est.VAR2))


#make the B matrix
loss2 <- function(param2){
  #Define the restriction
  B2 <- matrix(c(param2[1], param2[2], 0, param2[3]), ncol = 2)
  
  #Make BB' approximatively equal to omega
  X2 <- Omega2 - B2 %*% t(B2)
  
  #loss function
  loss2 <- sum(X2^2)
  return(loss2)
}

res.opt2 <- optim(c(1, 0, 1), loss2, method = "BFGS")
B.hat2 <- matrix(c(res.opt2$par[1], res.opt2$par[2], 0, res.opt2$par[3]), ncol = 2)

print(cbind(Omega2,B.hat2 %*% t(B.hat2)))


#get back the coefficient of est.VAR
phi2 <- Acoef(est.VAR2)
PHI2 = make.PHI(phi2)

#take the constant
constant2 <- sapply(est.VAR2$varresult, function(eq) coef(eq)["const"])
c2=as.matrix(constant2)

#Simulate the IRF
p2 <- length(phi2)
n2 <- dim(phi2[[1]])[1]

par(mfrow=c(2,2))
Y2 <- simul.VAR(c=c2, Phi = phi2, B = B.hat2, nb.sim ,y0.star=rep(0, n2*p2),
               indic.IRF = 1, u.shock = c(1,0))
par(mfrow=c(1,1))  
par(plt=c(0.1, 0.9, 0.1, 0.9))
plot(Y2[,2],type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")


#cumulative effect of a shock
plot(cumsum(Y2[,2]),type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")



#does N granger cause vol
grangertest(y2[,c("N", "vol")], order = 17)

#does dum granger cause vol
grangertest(y2[,c("vol", "N")], order = 17)










#---------------------------------------------------------------------------------------------
#tarrif
y3 =cbind(Vdata$total_tariff, Vdata$SPY_vol)
colnames(y3)[1:2] <- c("tariff", "vol")
est.VAR3 <- VAR(y3,p=17)
summary(est.VAR3)
Omega3 <- var(residuals(est.VAR3))


#make the B matrix
loss3 <- function(param3){
  #Define the restriction
  B3 <- matrix(c(param3[1], param3[2], 0, param3[3]), ncol = 2)
  
  #Make BB' approximatively equal to omega
  X3 <- Omega3 - B3 %*% t(B3)
  
  #loss function
  loss3 <- sum(X3^2)
  return(loss3)
}

res.opt3 <- optim(c(1, 0, 1), loss3, method = "BFGS")
B.hat3 <- matrix(c(res.opt3$par[1], res.opt3$par[2], 0, res.opt3$par[3]), ncol = 2)

print(cbind(Omega3,B.hat3 %*% t(B.hat3)))


#get back the coefficient of est.VAR
phi3 <- Acoef(est.VAR3)
PHI3 = make.PHI(phi3)

#take the constant
constant3 <- sapply(est.VAR3$varresult, function(eq) coef(eq)["const"])
c3=as.matrix(constant3)

#Simulate the IRF
p3 <- length(phi3)
n3 <- dim(phi3[[1]])[1]

par(mfrow=c(2,2))
Y3 <- simul.VAR(c=c3, Phi = phi3, B = B.hat3, nb.sim ,y0.star=rep(0, n3*p3),
                indic.IRF = 1, u.shock = c(1,0))
par(mfrow=c(1,1))  
par(plt=c(0.1, 0.9, 0.1, 0.9))
plot(Y3[,2],type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")


#cumulative effect of a shock
plot(cumsum(Y3[,2]),type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")




#does vol granger cause tarrif
grangertest(y3[,c("vol","tariff")], order = 17)

#does tarrif granger cause vol
grangertest(y3[,c("tariff", "vol")], order = 17)

















#---------------------------------------------------------------------------------------------
#Trade
y4 = cbind(Vdata$total_trade, Vdata$SPY_vol)
colnames(y4)[1:2] <- c("trade", "vol")
est.VAR4 <- VAR(y4,p=6)
summary(est.VAR4)
Omega4 <- var(residuals(est.VAR4))


#make the B matrix
loss4 <- function(param4){
  #Define the restriction
  B4 <- matrix(c(param4[1], param4[2], 0, param4[3]), ncol = 2)
  
  #Make BB' approximatively equal to omega
  X4 <- Omega4 - B4 %*% t(B4)
  
  #loss function
  loss4 <- sum(X4^2)
  return(loss4)
}

res.opt4 <- optim(c(1, 0, 1), loss4, method = "BFGS")
B.hat4 <- matrix(c(res.opt4$par[1], res.opt4$par[2], 0, res.opt4$par[3]), ncol = 2)

print(cbind(Omega4,B.hat4 %*% t(B.hat4)))


#get back the coefficient of est.VAR
phi4 <- Acoef(est.VAR4)
PHI4 = make.PHI(phi4)

#take the constant
constant4 <- sapply(est.VAR4$varresult, function(eq) coef(eq)["const"])
c4=as.matrix(constant4)

#Simulate the IRF
p4 <- length(phi4)
n4 <- dim(phi4[[1]])[1]

par(mfrow=c(2,2))
Y4 <- simul.VAR(c=c4, Phi = phi4, B = B.hat4, nb.sim ,y0.star=rep(0, n4*p4),
                indic.IRF = 1, u.shock = c(1,0))
par(mfrow=c(1,1))  
par(plt=c(0.1, 0.9, 0.1, 0.9))
plot(Y4[,2],type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")


#cumulative effect of a shock
plot(cumsum(Y4[,2]),type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")




#does vol granger cause tarrif
grangertest(y4[,c("vol","trade")], order = 7)

#does tarrif granger cause vol
grangertest(y4[,c("trade", "vol")], order = 7)

#------------------------------------------------------------------------------------------------
ychina = cbind(Vdata$total_china, Vdata$SPY_vol)
colnames(ychina)[1:2] <- c("china", "vol")
est.VARchina <- VAR(ychina,p=7)
summary(est.VARchina)
Omegachina <- var(residuals(est.VARchina))


#make the B matrix
losschina <- function(paramchina){
  #Define the restriction
  Bchina <- matrix(c(paramchina[1], paramchina[2], 0, paramchina[3]), ncol = 2)
  
  #Make BB' approximatively equal to omega
  Xchina <- Omegachina - Bchina %*% t(Bchina)
  
  #loss function
  losschina <- sum(Xchina^2)
  return(losschina)
}

res.optchina <- optim(c(1, 0, 1), losschina, method = "BFGS")
B.hatchina <- matrix(c(res.optchina$par[1], res.optchina$par[2], 0, res.optchina$par[3]), ncol = 2)

print(cbind(Omegachina,B.hatchina %*% t(B.hatchina)))


#get back the coefficient of est.VAR
phichina <- Acoef(est.VARchina)
PHIchina = make.PHI(phichina)

#take the constant
constantchina <- sapply(est.VARchina$varresult, function(eq) coef(eq)["const"])
cchina=as.matrix(constantchina)

#Simulate the IRF
pchina <- length(phichina)
nchina <- dim(phichina[[1]])[1]

par(mfrow=c(2,2))
Ychina <- simul.VAR(c=cchina, Phi = phichina, B = B.hatchina, nb.sim ,y0.star=rep(0, nchina*pchina),
                    indic.IRF = 1, u.shock = c(1,0))
par(mfrow=c(1,1))  
par(plt=c(0.1, 0.9, 0.1, 0.9))
plot(Ychina[,2],type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")


#cumulative effect of a shock
plot(cumsum(Ychina[,2]),type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")















#---------------------------------------------------------------------------------------------------
##sentiment analysis(N)

Vdata2 = left_join(SPY_volatility, VGK_volatility, by="timestamp")
Vdata2 = left_join(Vdata2, ASHR_volatility, by="timestamp")


#positive
positive = dplyr::select(social_hourly, timestamp, adjusted_time, positive)
positive2 <- positive %>%
  group_by(adjusted_time) %>%
  summarise(positive = sum(positive))

Vdata2 <- Vdata2 %>%
  left_join(positive2, by = c("timestamp" = "adjusted_time"))
Vdata2$positive[is.na(Vdata2$positive)] = 0

#negative
negative = dplyr::select(social_hourly, timestamp, adjusted_time, negative)
negative2 <- negative %>%
  group_by(adjusted_time) %>%
  summarise(negative = sum(negative))
Vdata2 <- Vdata2 %>%
  left_join(negative2, by = c("timestamp" = "adjusted_time"))
Vdata2$negative[is.na(Vdata2$negative)] = 0

#trust
trust = dplyr::select(social_hourly, timestamp, adjusted_time, trust)
trust2 <- trust %>%
  group_by(adjusted_time) %>%
  summarise(trust = sum(trust))
Vdata2 <- Vdata2 %>%
  left_join(trust2, by = c("timestamp" = "adjusted_time"))
Vdata2$trust[is.na(Vdata2$trust)] = 0

#surprise
surprise = dplyr::select(social_hourly, timestamp, adjusted_time, surprise)
surprise2 <- surprise %>%
  group_by(adjusted_time) %>%
  summarise(surprise = sum(surprise))
Vdata2 <- Vdata2 %>%
  left_join(surprise2, by = c("timestamp" = "adjusted_time"))
Vdata2$surprise[is.na(Vdata2$surprise)] = 0

#sadness
sadness = dplyr::select(social_hourly, timestamp, adjusted_time, sadness)
sadness2 <- sadness %>%
  group_by(adjusted_time) %>%
  summarise(sadness = sum(sadness))
Vdata2 <- Vdata2 %>%
  left_join(sadness2, by = c("timestamp" = "adjusted_time"))
Vdata2$sadness[is.na(Vdata2$sadness)] = 0

#joy
joy = dplyr::select(social_hourly, timestamp, adjusted_time, joy)
joy2 <- joy %>%
  group_by(adjusted_time) %>%
  summarise(joy = sum(joy))
Vdata2 <- Vdata2 %>%
  left_join(joy2, by = c("timestamp" = "adjusted_time"))
Vdata2$joy[is.na(Vdata2$joy)] = 0

#fear
fear = dplyr::select(social_hourly, timestamp, adjusted_time, fear)
fear2 <- fear %>%
  group_by(adjusted_time) %>%
  summarise(fear = sum(fear))
Vdata2 <- Vdata2 %>%
  left_join(fear2, by = c("timestamp" = "adjusted_time"))
Vdata2$fear[is.na(Vdata2$fear)] = 0

#disgust
disgust = dplyr::select(social_hourly, timestamp, adjusted_time, disgust)
disgust2 <- disgust %>%
  group_by(adjusted_time) %>%
  summarise(disgust = sum(disgust))
Vdata2 <- Vdata2 %>%
  left_join(disgust2, by = c("timestamp" = "adjusted_time"))
Vdata2$disgust[is.na(Vdata2$disgust)] = 0

#anticipation
anticipation = dplyr::select(social_hourly, timestamp, adjusted_time, anticipation)
anticipation2 <- anticipation %>%
  group_by(adjusted_time) %>%
  summarise(anticipation = sum(anticipation))
Vdata2 <- Vdata2 %>%
  left_join(anticipation2, by = c("timestamp" = "adjusted_time"))
Vdata2$anticipation[is.na(Vdata2$anticipation)] = 0

#anger
anger = dplyr::select(social_hourly, timestamp, adjusted_time, anger)
anger2 <- anger %>%
  group_by(adjusted_time) %>%
  summarise(anger = sum(anger))
Vdata2 <- Vdata2 %>%
  left_join(anger2, by = c("timestamp" = "adjusted_time"))
Vdata2$anger[is.na(Vdata2$anger)] = 0


#rename volatility columns
names(Vdata2)[2] <- "SPY_vol"
names(Vdata2)[3] <- "VGK_vol"
names(Vdata2)[4] <- "ASHR_vol"






#POSITIVE


y5 = cbind(Vdata2$positive, Vdata2$SPY_vol)
colnames(y5)[1:2] <- c("positive", "vol")
est.VAR5 <- VAR(y5,p=6)
summary(est.VAR5)
Omega5 <- var(residuals(est.VAR5))


#make the B matrix
loss5 <- function(param5){
  #Define the restriction
  B5 <- matrix(c(param5[1], param5[2], 0, param5[3]), ncol = 2)
  
  #Make BB' approximatively equal to omega
  X5 <- Omega5 - B5 %*% t(B5)
  
  #loss function
  loss5 <- sum(X5^2)
  return(loss5)
}

res.opt5 <- optim(c(1, 0, 1), loss5, method = "BFGS")
B.hat5 <- matrix(c(res.opt5$par[1], res.opt5$par[2], 0, res.opt5$par[3]), ncol = 2)

print(cbind(Omega5,B.hat5 %*% t(B.hat5)))


#get back the coefficient of est.VAR
phi5 <- Acoef(est.VAR5)
PHI5 = make.PHI(phi5)

#take the constant
constant5 <- sapply(est.VAR5$varresult, function(eq) coef(eq)["const"])
c5=as.matrix(constant5)

#Simulate the IRF
p5 <- length(phi5)
n5 <- dim(phi5[[1]])[1]

par(mfrow=c(2,2))
Y5 <- simul.VAR(c=c5, Phi = phi5, B = B.hat5, nb.sim ,y0.star=rep(0, n5*p5),
                  indic.IRF = 1, u.shock = c(1,0))
par(mfrow=c(1,1))  
par(plt=c(0.1, 0.9, 0.1, 0.9))
plot(Y5[,2],type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")


#cumulative effect of a shock
plot(cumsum(Y5[,2]),type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")




#does vol granger cause tarrif
grangertest(y5[,c("vol","positive")], order = 7)

#does tarrif granger cause vol
grangertest(y5[,c("positive", "vol")], order = 7)











#negative


y6 = cbind(Vdata2$negative, Vdata2$SPY_vol)
colnames(y6)[1:2] <- c("negative", "vol")
est.VAR6 <- VAR(y6,p=7)
summary(est.VAR6)
Omega6 <- var(residuals(est.VAR6))


#make the B matrix
loss6 <- function(param6){
  #Define the restriction
  B6 <- matrix(c(param6[1], param6[2], 0, param6[3]), ncol = 2)
  
  #Make BB' approximatively equal to omega
  X6 <- Omega6 - B6 %*% t(B6)
  
  #loss function
  loss6 <- sum(X6^2)
  return(loss6)
}

res.opt6 <- optim(c(1, 0, 1), loss6, method = "BFGS")
B.hat6 <- matrix(c(res.opt6$par[1], res.opt6$par[2], 0, res.opt6$par[3]), ncol = 2)

print(cbind(Omega6,B.hat6 %*% t(B.hat6)))


#get back the coefficient of est.VAR
phi6 <- Acoef(est.VAR6)
PHI6 = make.PHI(phi6)

#take the constant
constant6 <- sapply(est.VAR6$varresult, function(eq) coef(eq)["const"])
c6=as.matrix(constant6)

#Simulate the IRF
p6 <- length(phi6)
n6 <- dim(phi6[[1]])[1]

par(mfrow=c(2,2))
Y6 <- simul.VAR(c=c6, Phi = phi6, B = B.hat6, nb.sim ,y0.star=rep(0, n6*p6),
                  indic.IRF = 1, u.shock = c(1,0))
par(mfrow=c(1,1))  
par(plt=c(0.1, 0.9, 0.1, 0.9))
plot(Y6[,2],type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")


#cumulative effect of a shock
plot(cumsum(Y6[,2]),type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")





#trust

y7 = cbind(Vdata2$trust, Vdata2$SPY_vol)
colnames(y7)[1:2] <- c("trust", "vol")
est.VAR7 <- VAR(y7,p=7)
summary(est.VAR7)
Omega7 <- var(residuals(est.VAR7))


#make the B matrix
loss7 <- function(param7){
  #Define the restriction
  B7 <- matrix(c(param7[1], param7[2], 0, param7[3]), ncol = 2)
  
  #Make BB' approximatively equal to omega
  X7 <- Omega7 - B7 %*% t(B7)
  
  #loss function
  loss7 <- sum(X7^2)
  return(loss7)
}

res.opt7 <- optim(c(1, 0, 1), loss7, method = "BFGS")
B.hat7 <- matrix(c(res.opt7$par[1], res.opt7$par[2], 0, res.opt7$par[3]), ncol = 2)

print(cbind(Omega7,B.hat7 %*% t(B.hat7)))


#get back the coefficient of est.VAR
phi7 <- Acoef(est.VAR7)
PHI7 = make.PHI(phi7)

#take the constant
constant7 <- sapply(est.VAR7$varresult, function(eq) coef(eq)["const"])
c7=as.matrix(constant7)

#Simulate the IRF
p7 <- length(phi7)
n7 <- dim(phi7[[1]])[1]

par(mfrow=c(2,2))
Y7 <- simul.VAR(c=c7, Phi = phi7, B = B.hat7, nb.sim ,y0.star=rep(0, n7*p7),
                  indic.IRF = 1, u.shock = c(1,0))
par(mfrow=c(1,1))  
par(plt=c(0.1, 0.9, 0.1, 0.9))
plot(Y7[,2],type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")


#cumulative effect of a shock
plot(cumsum(Y7[,2]),type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")



#surprise

y8 = cbind(Vdata2$surprise, Vdata2$SPY_vol)
colnames(y8)[1:2] <- c("surprise", "vol")
est.VAR8 <- VAR(y8,p=7)
summary(est.VAR8)
Omega8 <- var(residuals(est.VAR8))


#make the B matrix
loss8 <- function(param8){
  #Define the restriction
  B8 <- matrix(c(param8[1], param8[2], 0, param8[3]), ncol = 2)
  
  #Make BB' approximatively equal to omega
  X8 <- Omega8 - B8 %*% t(B8)
  
  #loss function
  loss8 <- sum(X8^2)
  return(loss8)
}

res.opt8 <- optim(c(1, 0, 1), loss8, method = "BFGS")
B.hat8 <- matrix(c(res.opt8$par[1], res.opt8$par[2], 0, res.opt8$par[3]), ncol = 2)

print(cbind(Omega8,B.hat8 %*% t(B.hat8)))


#get back the coefficient of est.VAR
phi8 <- Acoef(est.VAR8)
PHI8 = make.PHI(phi8)

#take the constant
constant8 <- sapply(est.VAR8$varresult, function(eq) coef(eq)["const"])
c8=as.matrix(constant8)

#Simulate the IRF
p8 <- length(phi8)
n8 <- dim(phi8[[1]])[1]

par(mfrow=c(2,2))
Y8 <- simul.VAR(c=c8, Phi = phi8, B = B.hat8, nb.sim ,y0.star=rep(0, n8*p8),
                  indic.IRF = 1, u.shock = c(1,0))
par(mfrow=c(1,1))  
par(plt=c(0.1, 0.9, 0.1, 0.9))
plot(Y8[,2],type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")


#cumulative effect of a shock
plot(cumsum(Y8[,2]),type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")




#sadness

y9 = cbind(Vdata2$sadness, Vdata2$SPY_vol)
colnames(y9)[1:2] <- c("sadness", "vol")
est.VAR9 <- VAR(y9,p=7)
summary(est.VAR9)
Omega9 <- var(residuals(est.VAR9))


#make the B matrix
loss9 <- function(param9){
  #Define the restriction
  B9 <- matrix(c(param9[1], param9[2], 0, param9[3]), ncol = 2)
  
  #Make BB' approximatively equal to omega
  X9 <- Omega9 - B9 %*% t(B9)
  
  #loss function
  loss9 <- sum(X9^2)
  return(loss9)
}

res.opt9 <- optim(c(1, 0, 1), loss9, method = "BFGS")
B.hat9 <- matrix(c(res.opt9$par[1], res.opt9$par[2], 0, res.opt9$par[3]), ncol = 2)

print(cbind(Omega9,B.hat9 %*% t(B.hat9)))


#get back the coefficient of est.VAR
phi9 <- Acoef(est.VAR9)
PHI9 = make.PHI(phi9)

#take the constant
constant9 <- sapply(est.VAR9$varresult, function(eq) coef(eq)["const"])
c9=as.matrix(constant9)

#Simulate the IRF
p9 <- length(phi9)
n9 <- dim(phi9[[1]])[1]

par(mfrow=c(2,2))
Y9 <- simul.VAR(c=c9, Phi = phi9, B = B.hat9, nb.sim ,y0.star=rep(0, n9*p9),
                  indic.IRF = 1, u.shock = c(1,0))
par(mfrow=c(1,1))  
par(plt=c(0.1, 0.9, 0.1, 0.9))
plot(Y9[,2],type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")


#cumulative effect of a shock
plot(cumsum(Y9[,2]),type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")




#joy

y10 = cbind(Vdata2$joy, Vdata2$SPY_vol)
colnames(y10)[1:2] <- c("joy", "vol")
est.VAR10 <- VAR(y10,p=6)
summary(est.VAR10)
Omega10 <- var(residuals(est.VAR10))


#make the B matrix
loss10 <- function(param10){
  #Define the restriction
  B10 <- matrix(c(param10[1], param10[2], 0, param10[3]), ncol = 2)
  
  #Make BB' approximatively equal to omega
  X10 <- Omega10 - B10 %*% t(B10)
  
  #loss function
  loss10 <- sum(X10^2)
  return(loss10)
}

res.opt10 <- optim(c(1, 0, 1), loss10, method = "BFGS")
B.hat10 <- matrix(c(res.opt10$par[1], res.opt10$par[2], 0, res.opt10$par[3]), ncol = 2)

print(cbind(Omega10,B.hat10 %*% t(B.hat10)))


#get back the coefficient of est.VAR
phi10 <- Acoef(est.VAR10)
PHI10 = make.PHI(phi10)

#take the constant
constant10 <- sapply(est.VAR10$varresult, function(eq) coef(eq)["const"])
c10=as.matrix(constant10)

#Simulate the IRF
p10 <- length(phi10)
n10 <- dim(phi10[[1]])[1]

par(mfrow=c(2,2))
Y10 <- simul.VAR(c=c10, Phi = phi10, B = B.hat10, nb.sim ,y0.star=rep(0, n10*p10),
                  indic.IRF = 1, u.shock = c(1,0))
par(mfrow=c(1,1))  
par(plt=c(0.1, 0.9, 0.1, 0.9))
plot(Y10[,2],type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")


#cumulative effect of a shock
plot(cumsum(Y10[,2]),type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")



#fear

y10 = cbind(Vdata2$fear, Vdata2$SPY_vol)
colnames(y10)[1:2] <- c("fear", "vol")
est.VAR10 <- VAR(y10,p=6)
summary(est.VAR10)
Omega10 <- var(residuals(est.VAR10))


#make the B matrix
loss10 <- function(param10){
  #Define the restriction
  B10 <- matrix(c(param10[1], param10[2], 0, param10[3]), ncol = 2)
  
  #Make BB' approximatively equal to omega
  X10 <- Omega10 - B10 %*% t(B10)
  
  #loss function
  loss10 <- sum(X10^2)
  return(loss10)
}

res.opt10 <- optim(c(1, 0, 1), loss10, method = "BFGS")
B.hat10 <- matrix(c(res.opt10$par[1], res.opt10$par[2], 0, res.opt10$par[3]), ncol = 2)

print(cbind(Omega10,B.hat10 %*% t(B.hat10)))


#get back the coefficient of est.VAR
phi10 <- Acoef(est.VAR10)
PHI10 = make.PHI(phi10)

#take the constant
constant10 <- sapply(est.VAR10$varresult, function(eq) coef(eq)["const"])
c10=as.matrix(constant10)

#Simulate the IRF
p10 <- length(phi10)
n10 <- dim(phi10[[1]])[1]

par(mfrow=c(2,2))
Y10 <- simul.VAR(c=c10, Phi = phi10, B = B.hat10, nb.sim ,y0.star=rep(0, n10*p10),
                  indic.IRF = 1, u.shock = c(1,0))
par(mfrow=c(1,1))  
par(plt=c(0.1, 0.9, 0.1, 0.9))
plot(Y10[,2],type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")


#cumulative effect of a shock
plot(cumsum(Y10[,2]),type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")




#disgust

y11 = cbind(Vdata2$disgust, Vdata2$SPY_vol)
colnames(y11)[1:2] <- c("disgust", "vol")
est.VAR11 <- VAR(y11,p=6)
summary(est.VAR11)
Omega11 <- var(residuals(est.VAR11))


#make the B matrix
loss11 <- function(param11){
  #Define the restriction
  B11 <- matrix(c(param11[1], param11[2], 0, param11[3]), ncol = 2)
  
  #Make BB' approximatively equal to omega
  X11 <- Omega11 - B11 %*% t(B11)
  
  #loss function
  loss11 <- sum(X11^2)
  return(loss11)
}

res.opt11 <- optim(c(1, 0, 1), loss11, method = "BFGS")
B.hat11 <- matrix(c(res.opt11$par[1], res.opt11$par[2], 0, res.opt11$par[3]), ncol = 2)

print(cbind(Omega11,B.hat11 %*% t(B.hat11)))


#get back the coefficient of est.VAR
phi11 <- Acoef(est.VAR11)
PHI11 = make.PHI(phi11)

#take the constant
constant11 <- sapply(est.VAR11$varresult, function(eq) coef(eq)["const"])
c11=as.matrix(constant11)

#Simulate the IRF
p11 <- length(phi11)
n11 <- dim(phi11[[1]])[1]

par(mfrow=c(2,2))
Y11 <- simul.VAR(c=c11, Phi = phi11, B = B.hat11, nb.sim ,y0.star=rep(0, n11*p11),
                  indic.IRF = 1, u.shock = c(1,0))
par(mfrow=c(1,1))  
par(plt=c(0.1, 0.9, 0.1, 0.9))
plot(Y11[,2],type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")


#cumulative effect of a shock
plot(cumsum(Y11[,2]),type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")





#anticipation

y15 = cbind(Vdata2$anticipation, Vdata2$SPY_vol)
colnames(y15)[1:2] <- c("anticipation", "vol")
est.VAR15 <- VAR(y15,p=6)
summary(est.VAR15)
Omega15 <- var(residuals(est.VAR15))


#make the B matrix
loss15 <- function(param15){
  #Define the restriction
  B15 <- matrix(c(param15[1], param15[2], 0, param15[3]), ncol = 2)
  
  #Make BB' approximatively equal to omega
  X15 <- Omega15 - B15 %*% t(B15)
  
  #loss function
  loss15 <- sum(X15^2)
  return(loss15)
}

res.opt15 <- optim(c(1, 0, 1), loss15, method = "BFGS")
B.hat15 <- matrix(c(res.opt15$par[1], res.opt15$par[2], 0, res.opt15$par[3]), ncol = 2)

print(cbind(Omega15,B.hat15 %*% t(B.hat15)))


#get back the coefficient of est.VAR
phi15 <- Acoef(est.VAR15)
PHI15 = make.PHI(phi15)

#take the constant
constant15 <- sapply(est.VAR15$varresult, function(eq) coef(eq)["const"])
c15=as.matrix(constant15)

#Simulate the IRF
p15 <- length(phi15)
n15 <- dim(phi15[[1]])[1]

par(mfrow=c(2,2))
y15 <- simul.VAR(c=c15, Phi = phi15, B = B.hat15, nb.sim ,y0.star=rep(0, n15*p15),
                  indic.IRF = 1, u.shock = c(1,0))
par(mfrow=c(1,1))  
par(plt=c(0.1, 0.9, 0.1, 0.9))
plot(y15[,2],type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")


#cumulative effect of a shock
plot(cumsum(y15[,2]),type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")





#anger

y16 = cbind(Vdata2$anger, Vdata2$SPY_vol)
colnames(y16)[1:2] <- c("anger", "vol")
est.VAR16 <- VAR(y16,p=6)
summary(est.VAR16)
Omega16 <- var(residuals(est.VAR16))


#make the B matrix
loss16 <- function(param16){
  #Define the restriction
  B16 <- matrix(c(param16[1], param16[2], 0, param16[3]), ncol = 2)
  
  #Make BB' approximatively equal to omega
  X16 <- Omega16 - B16 %*% t(B16)
  
  #loss function
  loss16 <- sum(X16^2)
  return(loss16)
}

res.opt16 <- optim(c(1, 0, 1), loss16, method = "BFGS")
B.hat16 <- matrix(c(res.opt16$par[1], res.opt16$par[2], 0, res.opt16$par[3]), ncol = 2)

print(cbind(Omega16,B.hat16 %*% t(B.hat16)))


#get back the coefficient of est.VAR
phi16 <- Acoef(est.VAR16)
PHI16 = make.PHI(phi16)

#take the constant
constant16 <- sapply(est.VAR16$varresult, function(eq) coef(eq)["const"])
c16=as.matrix(constant16)

#Simulate the IRF
p16 <- length(phi16)
n16 <- dim(phi16[[1]])[1]

par(mfrow=c(2,2))
Y16 <- simul.VAR(c=c16, Phi = phi16, B = B.hat16, nb.sim ,y0.star=rep(0, n16*p16),
                  indic.IRF = 1, u.shock = c(1,0))
par(mfrow=c(1,1))  
par(plt=c(0.1, 0.9, 0.1, 0.9))
plot(Y16[,2],type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")


#cumulative effect of a shock
plot(cumsum(Y16[,2]),type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")








#----------------------------------------------------------------------------------------------------------
#interaction
##N and tariff, 2 variables

int1 = Vdata$total_tariff * Vdata$N

y12 = cbind(int1, Vdata$SPY_vol)
colnames(y12)[1:2] <- c("interaction", "vol")
est.VAR12 <- VAR(y12,p=6)
summary(est.VAR12)
Omega12 <- var(residuals(est.VAR12))


#make the B matrix
loss12 <- function(param12){
  #Define the restriction
  B12 <- matrix(c(param12[1], param12[2], 0, param12[3]), ncol = 2)
  
  #Make BB' approximatively equal to omega
  X12 <- Omega12 - B12 %*% t(B12)
  
  #loss function
  loss12 <- sum(X12^2)
  return(loss12)
}

res.opt12 <- optim(c(1, 0, 1), loss12, method = "BFGS")
B.hat12 <- matrix(c(res.opt12$par[1], res.opt12$par[2], 0, res.opt12$par[3]), ncol = 2)

print(cbind(Omega12,B.hat12 %*% t(B.hat12)))


#get back the coefficient of est.VAR
phi12 <- Acoef(est.VAR12)
PHI12 = make.PHI(phi12)

#take the constant
constant12 <- sapply(est.VAR12$varresult, function(eq) coef(eq)["const"])
c12=as.matrix(constant12)

#Simulate the IRF
p12 <- length(phi12)
n12 <- dim(phi12[[1]])[1]

par(mfrow=c(2,2))
Y12 <- simul.VAR(c=c12, Phi = phi12, B = B.hat12, nb.sim ,y0.star=rep(0, n12*p12),
                  indic.IRF = 1, u.shock = c(1,0))
par(mfrow=c(1,1))  
par(plt=c(0.1, 0.9, 0.1, 0.9))
plot(Y12[,2],type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")


#cumulative effect of a shock
plot(cumsum(Y12[,2]),type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")






##N and tariff, 3 variables

y12.2 = cbind(Vdata$total_trade, int1)
y12.2 = cbind(y12.2, Vdata$SPY_vol)
colnames(y12.2)[1:3] <- c("tariff", "Ntariff", "vol")
est.VAR12.2 <- VAR(y12.2,p=6)
summary(est.VAR12.2)
Omega12.2 <- var(residuals(est.VAR12.2))


#make the B matrix
loss12.2 <- function(param12.2){
  #Define the restriction
  B12.2 <- matrix(c(param12.2[1], param12.2[2], param12.2[3], 0, param12.2[4],param12.2[5], 0, 0,param12.2[6]), ncol = 3)
  
  #Make BB' approximatively equal to omega
  X12.2 <- Omega12.2 - B12.2 %*% t(B12.2)
  
  #loss function
  loss12.2 <- sum(X12.2^2)
  return(loss12.2)
}

res.opt12.2 <- optim(c(1, 0, 0, 1, 0, 1), loss12.2, method = "BFGS")
B.hat12.2 <- matrix(c(res.opt12.2$par[1], res.opt12.2$par[2], res.opt12.2$par[3], 0, res.opt12.2$par[4], res.opt12.2$par[5], 0, 0, res.opt12.2$par[6]), ncol = 3)

print(cbind(Omega12.2,B.hat12.2 %*% t(B.hat12.2)))


#get back the coefficient of est.VAR
phi12.2 <- Acoef(est.VAR12.2)
PHI12.2 = make.PHI(phi12.2)

#take the constant
constant12.2 <- sapply(est.VAR12.2$varresult, function(eq) coef(eq)["const"])
c12.2=as.matrix(constant12.2)

#Simulate the IRF
p12.2 <- length(phi12.2)
n12.2 <- dim(phi12.2[[1]])[1]

par(mfrow=c(2,2))
y12.2.N <- simul.VAR(c=c12.2, Phi = phi12.2, B = B.hat12.2, nb.sim ,y0.star=rep(0, n12.2*p12.2),
                 indic.IRF = 1, u.shock = c(1,0,0))
y12.2.I <- simul.VAR(c=c12.2, Phi = phi12.2, B = B.hat12.2, nb.sim ,y0.star=rep(0, n12.2*p12.2),
                     indic.IRF = 1, u.shock = c(0,1,0))
par(mfrow=c(1,2))  
plot(y12.2.N[,3],type="l",lwd=2,xlab="",ylab="",main="N on Volatility")
plot(y12.2.I[,3],type="l",lwd=2,xlab="",ylab="",main="int on Volatility")


#cumulative effect of a shock
plot(cumsum(y12.2[,2]),type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")









#positive and negative
y13 = cbind(Vdata2$positive, Vdata2$negative)
y13 = cbind(y13, Vdata2$SPY_vol)
colnames(y13)[1:3] <- c("positive", "negative", "vol")
est.VAR13 <- VAR(y13,p=6)
summary(est.VAR13)
Omega13 <- var(residuals(est.VAR13))


#make the B matrix
loss13 <- function(param13){
  #Define the restriction
  B13 <- matrix(c(param13[1], param13[2], param13[3], 0, param13[4],param13[5], 0, 0,param13[6]), ncol = 3)
  
  #Make BB' approximatively equal to omega
  X13 <- Omega13 - B13 %*% t(B13)
  
  #loss function
  loss13 <- sum(X13^2)
  return(loss13)
}

res.opt13 <- optim(c(1, 0, 0, 1, 0, 1), loss13, method = "BFGS")
B.hat13 <- matrix(c(res.opt13$par[1], res.opt13$par[2], res.opt13$par[3], 0, res.opt13$par[4], res.opt13$par[5], 0, 0, res.opt13$par[6]), ncol = 3)

print(cbind(Omega13,B.hat13 %*% t(B.hat13)))


#get back the coefficient of est.VAR
phi13 <- Acoef(est.VAR13)
PHI13 = make.PHI(phi13)

#take the constant
constant13 <- sapply(est.VAR13$varresult, function(eq) coef(eq)["const"])
c13=as.matrix(constant13)

#Simulate the IRF
p13 <- length(phi13)
n13 <- dim(phi13[[1]])[1]

par(mfrow=c(2,2))
y13.N <- simul.VAR(c=c13, Phi = phi13, B = B.hat13, nb.sim ,y0.star=rep(0, n13*p13),
                     indic.IRF = 1, u.shock = c(1,0,0))
y13.I <- simul.VAR(c=c13, Phi = phi13, B = B.hat13, nb.sim ,y0.star=rep(0, n13*p13),
                     indic.IRF = 1, u.shock = c(0,1,0))
par(mfrow=c(1,2))  
plot(y13.N[,3],type="l",lwd=2,xlab="",ylab="",main="N on Volatility")
plot(y13.I[,3],type="l",lwd=2,xlab="",ylab="",main="int on Volatility")


#cumulative effect of a shock
plot(cumsum(y13[,2]),type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")


grangertest(y13[,c("positive","vol")], order = 6)
grangertest(y13[,c("negative","vol")], order = 6)
grangertest(y13[,c("vol","positive")], order = 6)
grangertest(y13[,c("vol","negative")], order = 6)

#seems to not have any granger causalities





#N and trade, 2 variables

int5 = Vdata$total_trade * Vdata$N

y14 = cbind(int5, Vdata$SPY_vol)
colnames(y14)[1:2] <- c("Ntrade", "vol")
est.VAR14 <- VAR(y14,p=6)
summary(est.VAR14)
Omega14 <- var(residuals(est.VAR14))


#make the B matrix
loss14 <- function(param14){
  #Define the restriction
  B14 <- matrix(c(param14[1], param14[2], 0, param14[3]), ncol = 2)
  
  #Make BB' approximatively equal to omega
  X14 <- Omega14 - B14 %*% t(B14)
  
  #loss function
  loss14 <- sum(X14^2)
  return(loss14)
}

res.opt14 <- optim(c(1, 0, 1), loss14, method = "BFGS")
B.hat14 <- matrix(c(res.opt14$par[1], res.opt14$par[2], 0, res.opt14$par[3]), ncol = 2)

print(cbind(Omega14,B.hat14 %*% t(B.hat14)))


#get back the coefficient of est.VAR
phi14 <- Acoef(est.VAR14)
PHI14 = make.PHI(phi14)

#take the constant
constant14 <- sapply(est.VAR14$varresult, function(eq) coef(eq)["const"])
c14=as.matrix(constant14)

#Simulate the IRF
p14 <- length(phi14)
n14 <- dim(phi14[[1]])[1]

par(mfrow=c(2,2))
Y14 <- simul.VAR(c=c14, Phi = phi14, B = B.hat14, nb.sim ,y0.star=rep(0, n14*p14),
                  indic.IRF = 1, u.shock = c(1,0))
par(mfrow=c(1,1))  
par(plt=c(0.1, 0.9, 0.1, 0.9))
plot(Y14[,2],type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")


#cumulative effect of a shock
plot(cumsum(Y14[,2]),type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")




#N and tariff, 3 variables
y14.2 = cbind(Vdata$total_trade, int5)
y14.2 = cbind(y14.2, Vdata$SPY_vol)
colnames(y14.2)[1:3] <- c("trade", "Ntrade", "vol")
est.VAR14.2 <- VAR(y14.2,p=6)
summary(est.VAR14.2)
Omega14.2 <- var(residuals(est.VAR14.2))


#make the B matrix
loss14.2 <- function(param14.2){
  #Define the restriction
  B14.2 <- matrix(c(param14.2[1], param14.2[2], param14.2[3], 0, param14.2[4],param14.2[5], 0, 0,param14.2[6]), ncol = 3)
  
  #Make BB' approximatively equal to omega
  X14.2 <- Omega14.2 - B14.2 %*% t(B14.2)
  
  #loss function
  loss14.2 <- sum(X14.2^2)
  return(loss14.2)
}

res.opt14.2 <- optim(c(1, 0, 0, 1, 0, 1), loss14.2, method = "BFGS")
B.hat14.2 <- matrix(c(res.opt14.2$par[1], res.opt14.2$par[2], res.opt14.2$par[3], 0, res.opt14.2$par[4], res.opt14.2$par[5], 0, 0, res.opt14.2$par[6]), ncol = 3)

print(cbind(Omega14.2,B.hat14.2 %*% t(B.hat14.2)))


#get back the coefficient of est.VAR
phi14.2 <- Acoef(est.VAR14.2)
PHI14.2 = make.PHI(phi14.2)

#take the constant
constant14.2 <- sapply(est.VAR14.2$varresult, function(eq) coef(eq)["const"])
c14.2=as.matrix(constant14.2)

#Simulate the IRF
p14.2 <- length(phi14.2)
n14.2 <- dim(phi14.2[[1]])[1]

y14.2.N <- simul.VAR(c=c14.2, Phi = phi14.2, B = B.hat14.2, nb.sim ,y0.star=rep(0, n14.2*p14.2),
                     indic.IRF = 1, u.shock = c(1,0,0))
y14.2.I <- simul.VAR(c=c14.2, Phi = phi14.2, B = B.hat14.2, nb.sim ,y0.star=rep(0, n14.2*p14.2),
                     indic.IRF = 1, u.shock = c(0,1,0))
par(mfrow=c(1,2))  
plot(y14.2.N[,3],type="l",lwd=2,xlab="",ylab="",main="N on Volatility")
plot(y14.2.I[,3],type="l",lwd=2,xlab="",ylab="",main="int on Volatility")


#cumulative effect of a shock
plot(cumsum(y14.2[,2]),type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")















#------------------------------------------------------------------------------------------------
##log(y) => dummy
ylog = cbind(Vdata$dummy, log(Vdata$SPY_vol))
colnames(ylog)[1:2] <- c("dummy", "vol")
est.VARlog <- VAR(ylog,p=6)
summary(est.VARlog)
Omegalog <- var(residuals(est.VARlog))


#make the B matrix
losslog <- function(paramlog){
  #Define the restriction
  Blog <- matrix(c(paramlog[1], paramlog[2], 0, paramlog[3]), ncol = 2)
  
  #Make BB' approximatively equal to omega
  Xlog <- Omegalog - Blog %*% t(Blog)
  
  #loss function
  losslog <- sum(Xlog^2)
  return(losslog)
}

res.optlog <- optim(c(1, 0, 1), losslog, method = "BFGS")
B.hatlog <- matrix(c(res.optlog$par[1], res.optlog$par[2], 0, res.optlog$par[3]), ncol = 2)

print(cbind(Omegalog,B.hatlog %*% t(B.hatlog)))


#get back the coefficient of est.VAR
philog <- Acoef(est.VARlog)
PHIlog = make.PHI(philog)

#take the constant
constantlog <- sapply(est.VARlog$varresult, function(eq) coef(eq)["const"])
clog=as.matrix(constantlog)

#Simulate the IRF
plog <- length(philog)
nlog <- dim(philog[[1]])[1]

par(mfrow=c(2,2))
Ylog <- simul.VAR(c=clog, Phi = philog, B = B.hatlog, nb.sim ,y0.star=rep(0, nlog*plog),
                  indic.IRF = 1, u.shock = c(1,0))
par(mfrow=c(1,1))  
par(plt=c(0.1, 0.9, 0.1, 0.9))
plot(Ylog[,2],type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")


#cumulative effect of a shock
plot(cumsum(Ylog[,2]),type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")


#granger causality
#we want the smaller p-value => h0 test if there is no causality
#does vol granger cause dum
grangertest(ylog[,c("vol","dum")], order = 6)

#does dum granger cause vol
grangertest(ylog[,c("dum", "vol")], order = 6)

#Variane decomposition
vd = fevd(Ylog, n.ahead = 20)
plot(vd)





#log(y3) => tariff
ylog2 = cbind(Vdata$total_tariff, log(Vdata$SPY_vol))
colnames(ylog2)[1:2] <- c("tariff", "vol")
est.VARlog2 <- VAR(ylog2,p=6)
summary(est.VARlog2)
Omegalog2 <- var(residuals(est.VARlog2))


#make the B matrix
losslog2 <- function(paramlog2){
  #Define the restriction
  Blog2 <- matrix(c(paramlog2[1], paramlog2[2], 0, paramlog2[3]), ncol = 2)
  
  #Make BB' approximatively equal to omega
  Xlog2 <- Omegalog2 - Blog2 %*% t(Blog2)
  
  #loss function
  losslog2 <- sum(Xlog2^2)
  return(losslog2)
}

res.optlog2 <- optim(c(1, 0, 1), losslog2, method = "BFGS")
B.hatlog2 <- matrix(c(res.optlog2$par[1], res.optlog2$par[2], 0, res.optlog2$par[3]), ncol = 2)

print(cbind(Omegalog2,B.hatlog2 %*% t(B.hatlog2)))


#get back the coefficient of est.VAR
philog2 <- Acoef(est.VARlog2)
PHIlog2 = make.PHI(philog2)

#take the constant
constantlog2 <- sapply(est.VARlog2$varresult, function(eq) coef(eq)["const"])
clog2=as.matrix(constantlog2)

#Simulate the IRF
plog2 <- length(philog2)
nlog2 <- dim(philog2[[1]])[1]

par(mfrow=c(2,2))
Ylog2 <- simul.VAR(c=clog2, Phi = philog2, B = B.hatlog2, nb.sim ,y0.star=rep(0, nlog2*plog2),
                  indic.IRF = 1, u.shock = c(1,0))
par(mfrow=c(1,1))  
par(plt=c(0.1, 0.9, 0.1, 0.9))
plot(Ylog2[,2],type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")


#cumulative effect of a shock
plot(cumsum(Ylog2[,2]),type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")





#granger causality
#we want the smaller p-value => h0 test if there is no causality
#does vol granger cause dum
grangertest(y[,c("vol","dum")], order = 7)

#does dum granger cause vol
grangertest(y[,c("dum", "vol")], order = 7)

#Variane decomposition
vd = fevd(Y, n.ahead = 20)
plot(vd)


#------------------------------------------------------------------------------------------------------
#Let's compare if Trump second term creates more volatility or not. 
#Second term has creates more (that's not an assumption), so we should see it

#with with tweet outside market hour
#first term
Vdata_f = filter(data,between(timestamp, as.Date('2017-01-20'), as.Date('2021-01-20')))

#second term
Vdata_s = filter(data,between(timestamp, as.Date('2025-01-20'), as.Date('2025-05-07')))

rm(Vdata_f)
rm(Vdata_s)

#or 


#without tweet outside market hour

#Data for first mandate
SPY_f = filter(SPY,between(timestamp, as.Date('2017-01-20'), as.Date('2021-01-20')))

## Volatility

#find hourly volatility
SPY_volatility_f = dplyr::select(SPY_f,timestamp,r_vol_h)

#aggregating per hour
SPY_volatility_f = SPY_volatility_f %>%
  mutate(timestamp = floor_date(timestamp, unit = "hour")) %>%
  distinct(timestamp, .keep_all = TRUE) 

Vdata_f = left_join(SPY_volatility_f, dplyr::select(tweetdummy, -adjusted_time), by="timestamp")
Vdata_f = left_join(Vdata_f, dplyr::select(tweetcount, -adjusted_time), by="timestamp")
Vdata_f = left_join(Vdata_f, dplyr::select(tariff, -adjusted_time), by="timestamp")
Vdata_f = left_join(Vdata_f, dplyr::select(trade, -adjusted_time), by="timestamp")
Vdata_f = left_join(Vdata_f, dplyr::select(china, -adjusted_time), by="timestamp")


#rename volatility columns
names(Vdata_f)[2] <- "SPY_vol"

#convert NA to zeroes
Vdata_f$N[is.na(Vdata_f$N)] = 0
Vdata_f$dummy[is.na(Vdata_f$dummy)] = 0
Vdata_f$total_trade[is.na(Vdata_f$total_trade)] = 0
Vdata_f$total_tariff[is.na(Vdata_f$total_tariff)] = 0
Vdata_f$total_china[is.na(Vdata_f$total_china)] = 0


#Data for Second mandate
SPY_s = filter(SPY,between(timestamp, as.Date('2025-01-20'), as.Date('2025-05-07')))


## Volatility

#find hourly volatility
SPY_volatility_s = dplyr::select(SPY_s,timestamp,r_vol_h)

#aggregating per hour
SPY_volatility_s = SPY_volatility_s %>%
  mutate(timestamp = floor_date(timestamp, unit = "hour")) %>%
  distinct(timestamp, .keep_all = TRUE) 

Vdata_s = left_join(SPY_volatility_s, dplyr::select(tweetdummy, -adjusted_time), by="timestamp")
Vdata_s = left_join(Vdata_s, dplyr::select(tweetcount, -adjusted_time), by="timestamp")
Vdata_s = left_join(Vdata_s, dplyr::select(tariff, -adjusted_time), by="timestamp")
Vdata_s = left_join(Vdata_s, dplyr::select(trade, -adjusted_time), by="timestamp")
Vdata_s = left_join(Vdata_s, dplyr::select(china, -adjusted_time), by="timestamp")


#rename volatility columns
names(Vdata_s)[2] <- "SPY_vol"

#convert NA to zeroes
Vdata_s$N[is.na(Vdata_s$N)] = 0
Vdata_s$dummy[is.na(Vdata_s$dummy)] = 0
Vdata_s$total_tariff[is.na(Vdata_s$total_tariff)] = 0
Vdata_s$total_trade[is.na(Vdata_s$total_trade)] = 0
Vdata_s$total_china[is.na(Vdata_s$total_china)] = 0






#dummy
y_test = cbind(Vdata_s$dummy, Vdata_s$SPY_vol)

y_lag = VARselect(y_test, lag.max = 70)


adf.test(Vdata_f$SPY_vol, k = 6)

adf.test(Vdata_s$SPY_vol, k = 6)






#First mandate with dummy
y_f_d = cbind(Vdata_f$total_china, Vdata_f$SPY_vol)
colnames(y_f_d)[1:2] <- c("dummy", "vol")
est.VAR_f_d <- VAR(y_f_d,p=6)
summary(est.VAR_f_d)
Omega_f_d <- var(residuals(est.VAR_f_d))


#make the B matrix
loss_f_d <- function(param_f_d){
  #Define the restriction
  B_f_d <- matrix(c(param_f_d[1], param_f_d[2], 0, param_f_d[3]), ncol = 2)
  
  #Make BB' approximatively equal to omega
  X_f_d <- Omega_f_d - B_f_d %*% t(B_f_d)
  
  #loss function
  loss_f_d <- sum(X_f_d^2)
  return(loss_f_d)
}

res.opt_f_d <- optim(c(1, 0, 1), loss_f_d, method = "BFGS")
B.hat_f_d <- matrix(c(res.opt_f_d$par[1], res.opt_f_d$par[2], 0, res.opt_f_d$par[3]), ncol = 2)

print(cbind(Omega_f_d,B.hat_f_d %*% t(B.hat_f_d)))


#get back the coefficient of est.VAR
phi_f_d <- Acoef(est.VAR_f_d)
PHI_f_d = make.PHI(phi_f_d)

#take the constant
constant_f_d <- sapply(est.VAR_f_d$varresult, function(eq) coef(eq)["const"])
c_f_d=as.matrix(constant_f_d)

#Simulate the IRF
p_f_d <- length(phi_f_d)
n_f_d <- dim(phi_f_d[[1]])[1]

Y_f_d <- simul.VAR(c=c_f_d, Phi = phi_f_d, B = B.hat_f_d, nb.sim ,y0.star=rep(0, n_f_d*p_f_d),
                  indic.IRF = 1, u.shock = c(1,0))
par(mfrow=c(1,1))  
par(plt=c(0.1, 0.9, 0.1, 0.9))
plot(Y_f_d[,2],type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")


#does vol granger cause dum
grangertest(y_f_d[,c("vol","dummy")], order = 6)

#does dum granger cause vol
grangertest(y_f_d[,c("dummy", "vol")], order = 6)






#Second with dummy

y_s_d = cbind(Vdata_s$total_china, Vdata_s$SPY_vol)
colnames(y_s_d)[1:2] <- c("dummy", "vol")
est.VAR_s_d <- VAR(y_s_d,p=6)
summary(est.VAR_s_d)
Omega_s_d <- var(residuals(est.VAR_s_d))


#make the B matrix
loss_s_d <- function(param_s_d){
  #Define the restriction
  B_s_d <- matrix(c(param_s_d[1], param_s_d[2], 0, param_s_d[3]), ncol = 2)
  
  #Make BB' approximatively equal to omega
  X_s_d <- Omega_s_d - B_s_d %*% t(B_s_d)
  
  #loss function
  loss_s_d <- sum(X_s_d^2)
  return(loss_s_d)
}

res.opt_s_d <- optim(c(1, 0, 1), loss_s_d, method = "BFGS")
B.hat_s_d <- matrix(c(res.opt_s_d$par[1], res.opt_s_d$par[2], 0, res.opt_s_d$par[3]), ncol = 2)

print(cbind(Omega_s_d,B.hat_s_d %*% t(B.hat_s_d)))


#get back the coefficient of est.VAR
phi_s_d <- Acoef(est.VAR_s_d)
PHI_s_d = make.PHI(phi_s_d)

#take the constant
constant_s_d <- sapply(est.VAR_s_d$varresult, function(eq) coef(eq)["const"])
c_s_d=as.matrix(constant_s_d)

#Simulate the IRF
p_s_d <- length(phi_s_d)
n_s_d <- dim(phi_s_d[[1]])[1]

par(mfrow=c(2,2))
Y_s_d <- simul.VAR(c=c_s_d, Phi = phi_s_d, B = B.hat_s_d, nb.sim ,y0.star=rep(0, n_s_d*p_s_d),
                  indic.IRF = 1, u.shock = c(1,0))
par(mfrow=c(1,1))  
par(plt=c(0.1, 0.9, 0.1, 0.9))
plot(Y_s_d[,2],type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")


#does vol granger cause dum
grangertest(y_s_d[,c("vol","dummy")], order = 6)

#does dum granger cause vol
grangertest(y_s_d[,c("dummy", "vol")], order = 6)
















#example
...
...
y... = cbind(Vdata$..., Vdata$SPY_vol)
colnames(y...)[1:2] <- c("...", "vol")
est.VAR... <- VAR(y...,p=6)
summary(est.VAR...)
Omega... <- var(residuals(est.VAR...))


#make the B matrix
loss... <- function(param...){
  #Define the restriction
  B... <- matrix(c(param...[1], param...[2], 0, param...[3]), ncol = 2)
  
  #Make BB' approximatively equal to omega
  X... <- Omega... - B... %*% t(B...)
  
  #loss function
  loss... <- sum(X...^2)
  return(loss...)
}

res.opt... <- optim(c(1, 0, 1), loss..., method = "BFGS")
B.hat... <- matrix(c(res.opt...$par[1], res.opt...$par[2], 0, res.opt...$par[3]), ncol = 2)

print(cbind(Omega...,B.hat... %*% t(B.hat...)))


#get back the coefficient of est.VAR
phi... <- Acoef(est.VAR...)
PHI... = make.PHI(phi...)

#take the constant
constant... <- sapply(est.VAR...$varresult, function(eq) coef(eq)["const"])
c...=as.matrix(constant...)

#Simulate the IRF
p... <- length(phi...)
n... <- dim(phi...[[1]])[1]

par(mfrow=c(2,2))
Y... <- simul.VAR(c=c..., Phi = phi..., B = B.hat..., nb.sim ,y0.star=rep(0, n...*p...),
                  indic.IRF = 1, u.shock = c(1,0))
par(mfrow=c(1,1))  
par(plt=c(0.1, 0.9, 0.1, 0.9))
plot(Y...[,2],type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")


#cumulative effect of a shock
plot(cumsum(Y...[,2]),type="l",lwd=2,xlab="",ylab="",main="Effect of XXX on XXX")




#test de robustness (we want them to be the morecloser to each other)
#0) granger
#1) interaction
#2) termes (interval temporel)
#3) different market
#4) different lag
#5) closed hour data


#regarder variance du marché sans trump (est ce que ca monte et descend bcp)




