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
3) VAR: Tweet Sentiment on Returns (FRED -> daily, not intraday)
-> Work on specification

5) GARCH: Tweet Dummy on Log Returns (because predicts volatility)
-> rip





