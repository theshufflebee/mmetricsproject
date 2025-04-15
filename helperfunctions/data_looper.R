# This function loops AlphaVantage's data calls 

#Necessary libraries
library(alphavantager)
#don't forget to load your API key

#--------------------------------------------------------------------------------

#input year as a number, and symbol as a string 
data_loop = function(year,symbol){
  year = paste(year)
  months = c("01","02","03","04","05","06","07","08","09","10","11","12")
  for (t in 1:length(months)) {
    date = paste(year, months[t], sep="-")
    data = av_get(symbol, av_fun="TIME_SERIES_INTRADAY",interval="1min",
                  adjusted="false", extended_hours="false",
                  month=date, outputsize="full")
    filename = paste(symbol,date,sep="-")
    filename = paste("~/", filename, sep="")
    filename = paste(filename,".csv",sep="")
    write.csv(data,filename)
  }}

#example: data_loop(2025,"SPY)
#will download all intraday day for 2025 as a csv table
#note that this cvs file will go to your documents folder