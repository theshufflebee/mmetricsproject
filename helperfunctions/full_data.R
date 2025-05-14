# The following script enables us to load all "intermediary" datasets,
# select / create our relevant variables and load it nicely
# To see documentation, go to data_management

#-------------------------------------------------------------------------------


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



#Volatility


#find hourly volatility SPY
SPY_volatility = dplyr::select(SPY,timestamp,r_vol_h)

#aggregating per hour
SPY_volatility = SPY_volatility %>%
  mutate(timestamp = floor_date(timestamp, unit = "hour")) %>%
  distinct(timestamp, .keep_all = TRUE) 

#find hourly volatility VGK
VGK_volatility = dplyr::select(VGK,timestamp,r_vol_h)

#aggregating per hour
VGK_volatility = VGK_volatility %>%
  mutate(timestamp = floor_date(timestamp, unit = "hour")) %>%
  distinct(timestamp, .keep_all = TRUE) 

#find hourly volatility ASHR
ASHR_volatility = dplyr::select(ASHR,timestamp,r_vol_h)

#aggregating per hour
ASHR_volatility = ASHR_volatility %>%
  mutate(timestamp = floor_date(timestamp, unit = "hour")) %>%
  distinct(timestamp, .keep_all = TRUE) 



#Social


#find tweet count
tweetcount = dplyr::select(social_hourly,timestamp,adjusted_time,N)

#for taking count of closed market hours
tweetcount2 <- tweetcount %>%
  group_by(adjusted_time) %>%
  summarise(N = sum(N))


#find dummy
tweetdummy = dplyr::select(social_hourly,timestamp,adjusted_time,dummy)

#for taking count of closed market hours
tweetdummy2 <- tweetdummy %>%
  group_by(adjusted_time) %>%
  summarise(dummy = sum(dummy)) 
#peculiar interpretation for dummy: if dummy>1 it means that there were x
#out-hours which had tweets in them


#find count
tariff = dplyr::select(social_hourly,timestamp,adjusted_time,total_tariff)

#for taking count of closed market hours
tariff2 <- tariff %>%
  group_by(adjusted_time) %>%
  summarise(tariff = sum(total_tariff))


#find count
trade = dplyr::select(social_hourly,timestamp,adjusted_time,total_trade)

#for taking count of closed market hours
trade2 <- trade %>%
  group_by(adjusted_time) %>%
  summarise(trade = sum(total_trade)) 


#find count
china = dplyr::select(social_hourly,timestamp,adjusted_time,total_china)

#for taking count of closed market hours
china2 <- china %>%
  group_by(adjusted_time) %>%
  summarise(china = sum(total_china)) 


#find counts
anger = dplyr::select(social_hourly,timestamp,adjusted_time,anger)
anticipation = dplyr::select(social_hourly,timestamp,adjusted_time,anticipation)
disgust = dplyr::select(social_hourly,timestamp,adjusted_time,disgust)
fear = dplyr::select(social_hourly,timestamp,adjusted_time,fear)
joy = dplyr::select(social_hourly,timestamp,adjusted_time,joy)
sadness = dplyr::select(social_hourly,timestamp,adjusted_time,sadness)
surprise = dplyr::select(social_hourly,timestamp,adjusted_time,surprise)
trust = dplyr::select(social_hourly,timestamp,adjusted_time,trust)
total_sentiment = dplyr::select(social_hourly,timestamp,adjusted_time,total_sentiment)

anger2 <- anger %>%
  group_by(adjusted_time) %>%
  summarise(anger = sum(anger)) 
anticipation2 <- anticipation %>%
  group_by(adjusted_time) %>%
  summarise(anticipation= sum(anticipation)) 
disgust2 <- disgust %>%
  group_by(adjusted_time) %>%
  summarise(disgust = sum(disgust)) 
fear2 <- fear %>%
  group_by(adjusted_time) %>%
  summarise(fear = sum(fear)) 
joy2 <- joy %>%
  group_by(adjusted_time) %>%
  summarise(joy = sum(joy)) 
sadness2 <- sadness %>%
  group_by(adjusted_time) %>%
  summarise(sadness = sum(sadness)) 
surprise2 <- surprise %>%
  group_by(adjusted_time) %>%
  summarise(surprise = sum(surprise)) 
trust2 <- trust %>%
  group_by(adjusted_time) %>%
  summarise(trust = sum(trust)) 
total_sentiment2 <- total_sentiment %>%
  group_by(adjusted_time) %>%
  summarise(total_sentiment = sum(total_sentiment)) 

#positive 
positive = dplyr::select(social_hourly,timestamp,adjusted_time,positive)

positive2 <- positive %>%
  group_by(adjusted_time) %>%
  summarise(positive = sum(positive)) 

#negative
negative = dplyr::select(social_hourly,timestamp,adjusted_time,negative)

negative2 <- negative %>%
  group_by(adjusted_time) %>%
  summarise(negative = sum(negative)) 

#total
total_posneg = dplyr::select(social_hourly,timestamp,adjusted_time,total_posneg)

total_posneg2 <- total_posneg %>%
  group_by(adjusted_time) %>%
  summarise(total_posneg = sum(total_posneg)) 



#merge our dependant and independant vars

#case 1: ignore tweets outside trading hours
armax_data = left_join(SPY_volatility, VGK_volatility, by="timestamp")
armax_data = left_join(armax_data, ASHR_volatility, by="timestamp")
armax_data = left_join(armax_data, dplyr::select(tweetdummy, -adjusted_time), by="timestamp")
armax_data = left_join(armax_data, dplyr::select(tweetcount, -adjusted_time), by="timestamp")
armax_data = left_join(armax_data, dplyr::select(tariff, -adjusted_time), by="timestamp")
armax_data = left_join(armax_data, dplyr::select(trade, -adjusted_time), by="timestamp")
armax_data = left_join(armax_data, dplyr::select(china, -adjusted_time), by="timestamp")

rm(armax_data)
#case 2: push tweets made outside market hours to the next open hour
armax_data = left_join(SPY_volatility, VGK_volatility, by="timestamp")
armax_data = left_join(armax_data, ASHR_volatility, by="timestamp")
armax_data <- armax_data %>%
  left_join(tweetdummy2, by = c("timestamp" = "adjusted_time")) 
armax_data <- armax_data %>%
  left_join(tweetcount2, by = c("timestamp" = "adjusted_time")) 
armax_data <- armax_data %>%
  left_join(tariff2, by = c("timestamp" = "adjusted_time")) 
armax_data <- armax_data %>%
  left_join(trade2, by = c("timestamp" = "adjusted_time")) 
armax_data <- armax_data %>%
  left_join(china2, by = c("timestamp" = "adjusted_time")) 

#load sentiment counts
armax_data <- armax_data %>%
  left_join(positive2, by = c("timestamp" = "adjusted_time")) 
armax_data <- armax_data %>%
  left_join(negative2, by = c("timestamp" = "adjusted_time")) 
armax_data <- armax_data %>%
  left_join(total_posneg2, by = c("timestamp" = "adjusted_time")) 
armax_data <- armax_data %>%
  left_join(anger2, by = c("timestamp" = "adjusted_time")) 
armax_data <- armax_data %>%
  left_join(anticipation2, by = c("timestamp" = "adjusted_time")) 
armax_data <- armax_data %>%
  left_join(disgust2, by = c("timestamp" = "adjusted_time")) 
armax_data <- armax_data %>%
  left_join(fear2, by = c("timestamp" = "adjusted_time")) 
armax_data <- armax_data %>%
  left_join(joy2, by = c("timestamp" = "adjusted_time")) 
armax_data <- armax_data %>%
  left_join(sadness2, by = c("timestamp" = "adjusted_time")) 
armax_data <- armax_data %>%
  left_join(surprise2, by = c("timestamp" = "adjusted_time")) 
armax_data <- armax_data %>%
  left_join(trust2, by = c("timestamp" = "adjusted_time")) 
armax_data <- armax_data %>%
  left_join(total_sentiment2, by = c("timestamp" = "adjusted_time")) 

#convert NA to zeroes
armax_data[is.na(armax_data)] <- 0

#get proportions
armax_data <- armax_data %>%
  mutate(
    prop_anger = anger / total_sentiment,
    prop_anticipation = anticipation / total_sentiment,
    prop_disgust = disgust / total_sentiment,
    prop_fear = fear / total_sentiment,
    prop_joy = joy / total_sentiment,
    prop_sadness = sadness / total_sentiment,
    prop_surprise = surprise / total_sentiment,
    prop_trust = trust / total_sentiment,
    prop_negative = negative / total_posneg,
    prop_positive = positive / total_posneg)

#convert NA to zeroes again (cause divided by zeroes)
armax_data[is.na(armax_data)] <- 0

#remove non-proportion sentiments
armax_data <- armax_data %>%
  dplyr::select(-anger, -anticipation, -disgust, -fear, -joy, -sadness, -surprise, -trust, 
         -total_sentiment, -positive, -negative, -total_posneg)

#rename volatility columns
names(armax_data)[2] <- "SPY_vol"
names(armax_data)[3] <- "VGK_vol"
names(armax_data)[4] <- "ASHR_vol"

#rename data for general analysis 
data = armax_data

#keep relevant data & all other data without removing functions
rm(list = setdiff(ls(), c("data", names(Filter(is.function, mget(ls()))))))









