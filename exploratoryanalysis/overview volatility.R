rm(list=ls())


library(readr)
library("haven")
library("sandwich")
library("tinytex")
library("vtable")
library("ggplot2")
library("ggrepel")
library("lmtest")
library("tidyverse")
library("plm")
library("clubSandwich")
library("stringr")
library("lubridate")
library("AEC")


#SPY
raw_SPY <- read.csv("data/market_data/SPY.csv")
view(raw_SPY)

SPY_weighted_stats <- raw_SPY %>%
  mutate(date_only = as.Date(timestamp)) %>%
  group_by(date_only) %>%
  mutate(
    volume_total = sum(volume),
    weight = volume / volume_total
  ) %>%
  mutate(
    weighted_close = weight * close
  ) %>%
  summarise(
    weighted_mean_close = sum(weighted_close, na.rm = TRUE),
    weighted_var_close = sum(weight * (close - sum(weighted_close))^2, na.rm = TRUE)
  )
view(SPY_weighted_stats)

#Weighted Mean
ggplot(SPY_weighted_stats, aes(x = date_only, y = weighted_mean_close)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred", size = 1.5) +
  labs(
    title = "Moyenne pondérée closing Price",
    x = "Date",
    y = "Moyenne pondérée closing Price"
  ) +
  theme_minimal()

#Weighted Variance
ggplot(SPY_weighted_stats, aes(x = date_only, y = weighted_var_close)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred", size = 1.5) +
  labs(
    title = "Variance pondérée closing Price",
    x = "Date",
    y = "Variance pondérée closing Price"
  ) +
  theme_minimal()



#DAX
raw_DAX <- read.csv("data/market_data/DAX.csv")
view(raw_DAX)

DAX_weighted_stats <- raw_DAX %>%
  mutate(date_only = as.Date(timestamp)) %>%
  group_by(date_only) %>%
  mutate(
    volume_total = sum(volume),
    weight = volume / volume_total
  ) %>%
  mutate(
    weighted_close = weight * close
  ) %>%
  summarise(
    weighted_mean_close = sum(weighted_close, na.rm = TRUE),
    weighted_var_close = sum(weight * (close - sum(weighted_close))^2, na.rm = TRUE)
  )
view(DAX_weighted_stats)

#Weighted Mean
ggplot(DAX_weighted_stats, aes(x = date_only, y = weighted_mean_close)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred", size = 1.5) +
  labs(
    title = "Moyenne pondérée closing Price",
    x = "Date",
    y = "Moyenne pondérée closing Price"
  ) +
  theme_minimal()

#Weighted Variance
ggplot(DAX_weighted_stats, aes(x = date_only, y = weighted_var_close)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred", size = 1.5) +
  labs(
    title = "Variance pondérée closing Price",
    x = "Date",
    y = "Variance pondérée closing Price"
  ) +
  theme_minimal()



#ONEQ
raw_ONEQ <- read.csv("data/market_data/ONEQ.csv")
view(raw_ONEQ)

ONEQ_weighted_stats <- raw_ONEQ %>%
  mutate(date_only = as.Date(timestamp)) %>%
  group_by(date_only) %>%
  mutate(
    volume_total = sum(volume),
    weight = volume / volume_total
  ) %>%
  mutate(
    weighted_close = weight * close
  ) %>%
  summarise(
    weighted_mean_close = sum(weighted_close, na.rm = TRUE),
    weighted_var_close = sum(weight * (close - sum(weighted_close))^2, na.rm = TRUE)
  )
view(ONEQ_weighted_stats)

#Weighted Mean
ggplot(ONEQ_weighted_stats, aes(x = date_only, y = weighted_mean_close)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred", size = 1.5) +
  labs(
    title = "Moyenne pondérée closing Price",
    x = "Date",
    y = "Moyenne pondérée closing Price"
  ) +
  theme_minimal()

#Weighted Variance
ggplot(ONEQ_weighted_stats, aes(x = date_only, y = weighted_var_close)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred", size = 1.5) +
  labs(
    title = "Variance pondérée closing Price",
    x = "Date",
    y = "Variance pondérée closing Price"
  ) +
  theme_minimal()



#SMI
raw_SMI <- read.csv("data/market_data/SMI.csv")
view(raw_SMI)

SMI_weighted_stats <- raw_SMI %>%
  mutate(date_only = as.Date(timestamp)) %>%
  group_by(date_only) %>%
  mutate(
    volume_total = sum(volume),
    weight = volume / volume_total
  ) %>%
  mutate(
    weighted_close = weight * close
  ) %>%
  summarise(
    weighted_mean_close = sum(weighted_close, na.rm = TRUE),
    weighted_var_close = sum(weight * (close - sum(weighted_close))^2, na.rm = TRUE)
  )
view(SMI_weighted_stats)

#Weighted Mean
ggplot(SMI_weighted_stats, aes(x = date_only, y = weighted_mean_close)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred", size = 1.5) +
  labs(
    title = "Moyenne pondérée closing Price",
    x = "Date",
    y = "Moyenne pondérée closing Price"
  ) +
  theme_minimal()

#Weighted Variance
ggplot(SMI_weighted_stats, aes(x = date_only, y = weighted_var_close)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred", size = 1.5) +
  labs(
    title = "Variance pondérée closing Price",
    x = "Date",
    y = "Variance pondérée closing Price"
  ) +
  theme_minimal()




#ASHR
raw_ASHR <- read.csv("data/market_data/ASHR.csv")
view(raw_ASHR)

ASHR_weighted_stats <- raw_ASHR %>%
  mutate(date_only = as.Date(timestamp)) %>%
  group_by(date_only) %>%
  mutate(
    volume_total = sum(volume),
    weight = volume / volume_total
  ) %>%
  mutate(
    weighted_close = weight * close
  ) %>%
  summarise(
    weighted_mean_close = sum(weighted_close, na.rm = TRUE),
    weighted_var_close = sum(weight * (close - sum(weighted_close))^2, na.rm = TRUE)
  )
view(ASHR_weighted_stats)

#Weighted Mean
ggplot(ASHR_weighted_stats, aes(x = date_only, y = weighted_mean_close)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred", size = 1.5) +
  labs(
    title = "Moyenne pondérée closing Price",
    x = "Date",
    y = "Moyenne pondérée closing Price"
  ) +
  theme_minimal()

#Weighted Variance
ggplot(ASHR_weighted_stats, aes(x = date_only, y = weighted_var_close)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkred", size = 1.5) +
  labs(
    title = "Variance pondérée closing Price",
    x = "Date",
    y = "Variance pondérée closing Price"
  ) +
  theme_minimal()



#Engle Test, for SPY index 
#(knowing if volatility of SPY index encompass a heteroscedasticity on the shock)

T = nrow(raw_SPY)
Pute = 100*c(NaN, log(raw_SPY$close[2:T]/raw_SPY$close[1:T-1]))
u = Pute^2
u_1 = c(NaN, Pute[1:(T-1)]^2)
u_2 = c(NaN, NaN, Pute[1:(T-2)]^2)
eq = lm(u^2 ~ u_1^2 + u_2^2)
test.stat = length(u)*summary(eq)$r.squared
pvalue = 1 - pchisq(q = test.stat, df=2)

pvalue
#pvalue is close to 0 (BUT HIGHER THAN 0.05??????), so ?????we reject the null hypothesis?????? stating 
#that SPY do not encompass heteroscedasticity on the shock)



#Langrange Multiplier Test
raw_SPY$close <- ts(raw_SPY$close)
raw_SPY$timestamp <- seq.Date(as.Date('2025-03-11'), by = 'day', length.out = length(raw_SPY$close))
ggplot(raw_SPY, aes(y = close, x = timestamp )) + geom_line(col = 'red') +
  labs(title = 'BYD Daily Stock Returns', ylab = 'return')

library(FinTS)
byd.archTest <- ArchTest(raw_SPY$close, lags = 1, demean = TRUE)
byd.archTest
#Because the pvalue is <0.05, we reject the null hypothesis and conclude the presence of ARCH(1) effects.
#We are now sur to use ARCH model for this process


#estimation of ARCH
#To perform the test for ARCH effects, we must



library(fGarch)
arch.fit <- garchFit(~garch(1,0), data = raw_SPY$close, trace = F)
summary(arch.fit)



