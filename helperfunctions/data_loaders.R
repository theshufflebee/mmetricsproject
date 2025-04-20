# This file contains all functions related downloading and then importing 
#chunks of data

#Required libraries:
library(here)
library(dplyr)
library(vroom)
library(alphavantager)
#don't forget to load your API key for alphavantage

#--------------------------------------------------------------------------------
#-----------------                     1                        -----------------
#--------------------------------------------------------------------------------

# This function loops AlphaVantage's data calls 

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



#--------------------------------------------------------------------------------
#-----------------                     2                        -----------------
#--------------------------------------------------------------------------------

# This function loads every month of a symbol's datasets separately

#--------------------------------------------------------------------------------

#input year as number
#input months as span, i.e. 1:3 would be months 01 to 03
#input symbol as string, e.g. "SPY"

data_loader_months = function(year,months,symbol){
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



#--------------------------------------------------------------------------------
#-----------------                     3                        -----------------
#--------------------------------------------------------------------------------

# This function loads every month of a symbol's datasets and binds them to 
# have one dataset per symbol per year

#--------------------------------------------------------------------------------

#input year as number
#input months as span, i.e. 1:3 would be months 01 to 03
#input symbol as string, e.g. "SPY"

data_loader_year = function(year,months,symbol){
  paths <- here("data", "market_data", paste(symbol, 
                                             sprintf("%04d-%02d", year, months), sep = "-"))
  
  files <- paste0(paths, ".csv")
  
  combined_data <- bind_rows(vroom(files))
  
  assign(paste("raw",symbol,year,sep="_"), combined_data, envir = .GlobalEnv)
}

#example: data_loader2(year=2021,months=1:6,symbol="SPY")
#this would import all csv files for 2021, for each month from january to june
#and combine them into one dataset



#--------------------------------------------------------------------------------
#-----------------                     4                        -----------------
#--------------------------------------------------------------------------------

# This function loads every month of every year of a symbol's datasets 
#and binds them to have one full dataset per symbol 

#--------------------------------------------------------------------------------



# Function to load and merge all files with the same symbol
data_loader <- function(symbol){
  paths <- here("data", "market_data") 
  
  #list all CSV files that contain the symbol 
  files <- list.files(paths, pattern = paste(symbol, "-", sep=""), full.names = TRUE)
  
  #check if files found
  if (length(files) == 0) {
    stop("No files found for the symbol: ", symbol)
  }
  
  #read and combine
  combined_data <- bind_rows(lapply(files, function(file) {
    read.csv(file)
  }))
  
  #name it as raw_SYMBOL 
  assign(paste("raw", symbol, sep = "_"), combined_data, envir = .GlobalEnv)
}

#example: data_loader_all_sheets("SPY")
#this will load all CSV files containing "SPY" in their filename and merge them




