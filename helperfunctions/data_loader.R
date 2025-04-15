# This function loads every month of a symbol's datasets

#--------------------------------------------------------------------------------

#input year as number
#input months as span, i.e. 1:3 would be months 01 to 03
#input symbol as string, e.g. "SPY"

data_loader = function(year,months,symbol){
  year = paste(year) #convert to string
  months = sprintf("%02d",months) #convert to string and add 0 if need
  for (t in 1:length(months)) {
    date = paste(year, months[t], sep="-")
    Rdate = paste(year, months[t], sep="_")
    dataname = paste(symbol,date,sep="-")
    filename = paste(dataname,".csv",sep="")
    Rdataname = paste(symbol,Rdate,sep="_")
    assign(paste("raw",Rdataname,sep="_"),
           read.csv(here("data/market_data", filename)), envir = .GlobalEnv)
  }}

#example: data_loader(year=2021,months=1:6,symbol="SPY")
#this would import all csv files for 2021, for each month from january to june