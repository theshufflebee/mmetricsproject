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

Models:

Question: Calculate Realised Volatility then use ARMA/VAR or calculate Returns and use GARCH?

1) ARMAX: Tweet Count & Tweet Dummy on Volatility 
-> Question: How to select lags? select_armax uses AIC but prof uses regression and checks significance? Then use AIC?
-> Tweets really exogenous? Doubt

3) VAR: Tweet Sentiment on Returns (FRED -> daily, not intraday)
-> Not useful 

5) GARCH: Tweet Dummy on Log Returns (because predicts volatility)
->





