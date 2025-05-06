# mmetricsproject
Project for UNIL - HEC Macroeconometrics Class
--------------------

Commit Tutorial:

1) Pull

2) Edit (while often saving)

3) Commit changes

4) Pull

5) Push

--------------------

Data: 
-Tweets & Truths cleaned + sentiments
-SPY intraday prices + volatility hourly and daily averages
-> Move Tweets outside trading hours

Models:


1) ARMAX: Tweet Count & Tweet Dummy on Volatility 
-> Question: How to select lags? select_armax uses AIC but prof uses regression and checks significance? Then use AIC?
   Prof says can use both 
-> Tweets really exogenous? Doubt
   On volatility? Possibly
-> Question: What to do for 2021-2022?
-> r_vol_h values very low, ask prof what he thinks
-> Add interaction term count*tariff

3) VAR: Tweet Sentiment on Returns 
-> Work on specification
-> Plenty of lags 'cause lags?

4) GARCH: Tweet Dummy on Log Returns (because predicts volatility)
-> rip


To do:
- Run randomforests on volatility find words
- Stop ignoring tweets outside hours and include them in next hour
- Redownload april data
- Rename variables for final latex tables
- IRF for base (HAC) function (just plot the coefs for each lag)
- IRF for armax (and hopefully the two IRFs match)



