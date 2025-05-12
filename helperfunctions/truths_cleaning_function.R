


truths_processer_2 <- function(raw_data) {
  
  # Load the needed packages for the tokenization
  library(dplyr)
  library(stringr)
  library(lubridate)
  library(tidytext)
  library(textstem)
  
  data("stop_words")
  stop_words_list <- stop_words$word
  
  processed_data <- raw_data %>%
    
    # Filters out retweets
    filter(str_starts(x, "Donald")) %>%
    
    mutate(
      # Extract the full date and time
      date_time = str_extract(x, "\\b[A-Za-z]+ \\d{1,2}, \\d{4}, \\d{1,2}:\\d{2} [APM]{2}\\b"),
      
      # Convert 'date_time' to proper Date-Time format
      date_time_parsed = mdy_hm(date_time),  # Use mdy_hm() to parse the datetime
      
      # Extract date only for plot
      day = as.Date(date_time_parsed),
      
      # Extract time only for plot
      time = format(date_time_parsed, "%H:%M"),
      
      # Convert time to numeric hours & minutes as fractions
      time_numeric = hour(date_time_parsed) + minute(date_time_parsed) / 60,
      
      # Shift time such that y = 0 corresponds to 12 PM
      time_shifted = time_numeric - 12,
      
      # Extract post text after timestamp
      post = str_trim(str_remove(x, ".*\\d{1,2}, \\d{4}, \\d{1,2}:\\d{2} [APM]{2}\\s*")),
      
      # Remove "Original Post" string if present
      post = str_remove(post, "^Original Post\\s*"),
      
      # Binary variable for media-only posts
      media = if_else(post == "", 1, 0),
      
      # Binary variable for URLs
      link = if_else(str_detect(post, "^https"), 1, 0),
      
      # Remove URLs
      post = str_replace(post, "(http[s]?://|www\\.)\\S+", ""),
      
      # Lowercase post
      post_lower = str_to_lower(post),
      
      # Tokenize the tweets
      tokens = post_lower %>%
        str_replace_all("[^a-z\\s]", " ") %>%
        str_split("\\s+") %>%
        lapply(function(words) {
          words <- words[words != ""]  # Remove empty strings
          words <- setdiff(words, stop_words_list)  # Remove stopwords
          words <- lemmatize_words(words) # Lemmatize
        })
    )
  
  return(processed_data)
}


detect_words <- function(data, words, column) {
  #inpot a df and 
  
  # Loop through the words and create a binary column for each one
  for (word in words) {
    # Create a new column with binary indicator (1 if word is present, 0 if not)
    data <- data %>%
      mutate(!!paste0("contains_", word) := if_else(str_detect(colum, word), 1, 0))
  }
  return(data)
}
