# 1. Load Political 

#twitter
tweets <- read.csv(here("data/mothership", "tweets.csv"))

#truthsocial
truths <- read.csv(here("data/mothership", "truths.csv"))


# 2. Load Financial

#S&P500
SPY <- read.csv(here("data/mothership", "SPY.csv"))

#STOXX50
VGK <- read.csv(here("data/mothership", "VGK.csv"))

#CSI 300 (China)
ASHR <- read.csv(here("data/mothership", "ASHR.CSV"))


# 3. Merge

#mothership base (dataframe with each minute since 2010)
mothership <- data.frame(timestamp = seq(as.POSIXct("2019-01-01"),
                                         as.POSIXct("2020-04-16"), 
                                         by=(1*60)))

#make posixct
SPY$timestamp = as.POSIXct(SPY$timestamp,format = "%Y-%m-%d %H:%M:%S")
VGK$timestamp = as.POSIXct(VGK$timestamp,format = "%Y-%m-%d %H:%M:%S")
ASHR$timestamp = as.POSIXct(ASHR$timestamp,format = "%Y-%m-%d %H:%M:%S")
tweets$timestamp = as.POSIXct(tweets$timestamp,format = "%Y-%m-%d %H:%M:%S")
truths$timestamp = as.POSIXct(truths$timestamp,format = "%Y-%m-%d %H:%M:%S")


#merge with financial and socialmedia
mothership = left_join(mothership, SPY, by = "timestamp")
mothership = left_join(mothership, VGK, by = "timestamp")
mothership = left_join(mothership, ASHR, by = "timestamp")
mothership = left_join(mothership, tweets, by = "timestamp")
mothership = left_join(mothership, truths, by = "timestamp")




#remove intermediary steps
rm(tweets,truths,SPY,VGK,ASHR)
