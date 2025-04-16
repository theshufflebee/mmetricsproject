# This function loads every month of a symbol's datasets and binds them to 
# have one dataset per symbol per year

#Required libraries:
library(here)
library(dplyr)
library(vroom)

#--------------------------------------------------------------------------------

#input year as number
#input months as span, i.e. 1:3 would be months 01 to 03
#input symbol as string, e.g. "SPY"

data_loader2 = function(year,months,symbol){
  paths <- here("data", "market_data", paste(symbol, 
                                sprintf("%04d-%02d", year, months), sep = "-"))
  
  files <- paste0(paths, ".csv")
  
  combined_data <- bind_rows(vroom(files))
  
  assign(paste("raw",symbol,year,sep="_"), combined_data, envir = .GlobalEnv)
}

#example: data_loader2(year=2021,months=1:6,symbol="SPY")
#this would import all csv files for 2021, for each month from january to june
#and combine them into one dataset