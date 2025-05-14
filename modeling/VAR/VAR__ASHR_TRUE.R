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
Vdata = left_join(Vdata, select(tweetdummy, -adjusted_time), by="timestamp")
Vdata = left_join(Vdata, select(tweetcount, -adjusted_time), by="timestamp")
Vdata = left_join(Vdata, select(tariff, -adjusted_time), by="timestamp")
Vdata = left_join(Vdata, select(trade, -adjusted_time), by="timestamp")
Vdata = left_join(Vdata, select(china, -adjusted_time), by="timestamp")
Vdata = left_join(Vdata, select(positive, -adjusted_time), by="timestamp")
Vdata = left_join(Vdata, select(negative, -adjusted_time), by="timestamp")

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




Vdata <- Vdata %>%
  filter(!is.na(ASHR_vol))



#------------------------------------------------------------------------------------------------
#check the data  
##stationarity

###acf/pacf
acf(Vdata$SPY_vol)
pacf(Vdata$SPY_vol)

acf(Vdata$dummy)
pacf(Vdata$dummy)

####adf test (for stationarity of the outcome data) 
####p-value test H0, the probability that the process is not stationary
adf.test(Vdata$SPY_vol, k = 7)
adf.test(Vdata$dummy, k = 7)







#series
pro_v = ts(Vdata$ASHR_vol)
pro_t_dummy = ts(Vdata$dummy)

autoplot(pro_v)
autoplot(pro_t_dummy)









#-----------------------------------------------------------------------------------------
#Nombre of lag
##with dummy
y = cbind(Vdata$dummy, Vdata$ASHR_vol)

y_lag = VARselect(y, lag.max = 20)
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
  labs(title = "hourly Returns AIC",x="n" , y = "AIC") +
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
  labs(title = "hourly Returns HQ",x="dum" , y = "HQ") +
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


#dummy
colnames(y)[1:2] <- c("dum", "vol")
est.VAR <- VAR(y,p=7)
summary(est.VAR)

#-----------------------------------------------------------------------------------------------
#bunch of test 

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







y = cbind(Vdata$dummy, Vdata$ASHR_vol)
colnames(y)[1:2] <- c("dum", "vol")
est.VAR <- VAR(y,p=7)
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
nb.sim <- 40
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
grangertest(y[,c("vol","dum")], order = 7)

#does dum granger cause vol
grangertest(y[,c("dum", "vol")], order = 7)

#Variane decomposition
vd = fevd(Y, n.ahead = 20)
plot(vd)





#with N

y2 = cbind(Vdata$N, Vdata$ASHR_vol)
colnames(y2)[1:2] <- c("N", "vol")
est.VAR2 <- VAR(y2,p=7)
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
grangertest(y2[,c("N", "vol")], order = 7)

#does dum granger cause vol
grangertest(y2[,c("vol", "N")], order = 7)










#---------------------------------------------------------------------------------------------
#tarrif
y3 =cbind(Vdata$total_tariff, Vdata$ASHR_vol)
colnames(y3)[1:2] <- c("tariff", "vol")
est.VAR3 <- VAR(y3,p=6)
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
grangertest(y3[,c("vol","tariff")], order = 7)

#does tarrif granger cause vol
grangertest(y3[,c("tariff", "vol")], order = 7)

















#---------------------------------------------------------------------------------------------
#Trade
y4 = cbind(Vdata$total_trade, Vdata$ASHR_vol)
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










#China
## Number of Tweets Mentioning China 
#find count
chinaTest = dplyr::select(social_hourly,timestamp,total_china)


Vdata = left_join(Vdata, chinaTest, by="timestamp")
Vdata$total_china[is.na(Vdata$total_china)] = 0
Vdata <- Vdata %>%
  filter(!is.na(ASHR_vol))





ychina = cbind(Vdata$china, Vdata$SPY_vol)
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

Vdata2 <- Vdata2 %>%
  filter(!is.na(ASHR_vol))

#Use data in previous ASHR for not using adjusted (select commande didn't work here)
#Vdata2$ASHR_vol = Vdata2$r_vol_h

#POSITIVE


y5 = cbind(Vdata2$positive, Vdata2$ASHR_vol)
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


y6 = cbind(Vdata2$negative, Vdata2$ASHR_vol)
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

y7 = cbind(Vdata2$trust, Vdata2$ASHR_vol)
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

y8 = cbind(Vdata2$surprise, Vdata2$ASHR_vol)
colnames(y8)[1:2] <- c("surprise", "vol")
est.VAR8 <- VAR(y8,p=6)
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

y9 = cbind(Vdata2$sadness, Vdata2$ASHR_vol)
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

y10 = cbind(Vdata2$joy, Vdata2$ASHR_vol)
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

y10 = cbind(Vdata2$fear, Vdata2$ASHR_vol)
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

y11 = cbind(Vdata2$disgust, Vdata2$ASHR_vol)
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

y12 = cbind(Vdata2$anticipation, Vdata2$ASHR_vol)
colnames(y12)[1:2] <- c("anticipation", "vol")
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





#anger

y11 = cbind(Vdata2$anger, Vdata2$ASHR_vol)
colnames(y11)[1:2] <- c("anger", "vol")
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

#----------------------------------------------------------------------------------------------------------







#example
...
...
y... = cbind(Vdata$..., Vdata$SPY_vol)
colnames(y...)[1:2] <- c("...", "vol")
est.VAR... <- VAR(y...,p=7)
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







