# This function scrapes Trumps Truth Social Posts
# Use the second page URL (Couldn't be bothered to the special case for the first Page)

"https://trumpstruth.org/?sort=desc&per_page=50&cursor=eyJzdGF0dXNfY3JlYXRlZF9hdCI6IjIwMjUtMDMtMjMgMTc6MzQ6MjMiLCJfcG9pbnRzVG9OZXh0SXRlbXMiOnRydWV9"

--------------------------------------------------------------------------------

trumpstruthscraper <- function(num_pages, link) {
  current_url <- link
  all_quotes <- c()  # Store all quotes
  
  for (i in 1:num_pages) {  
    result <- scrape_page(current_url)
    
    # add to the end
    all_quotes <- c(all_quotes, result$quotes)
    
    # Stop if no next page is found
    if (is.na(result$next_page_url) || result$next_page_url == "") {
      break
    } else {
      current_url <- result$next_page_url  # Move to the next page
    }
    
    Sys.sleep(2)  # delays each request by 2 second
  }
  
  return(all_quotes)
}

# Scraper function for a single page -> returns scraped data and next page url
scrape_page <- function(url) {
  webpage <- read_html(url)
  
  # Extract quotes for class status on webpage
  quotes_raw <- webpage %>%
    html_nodes(".status") %>%
    html_text()
  
  # Removes some html format types
  quotes_clean <- quotes_raw %>%
    str_replace_all("[\r\n\t]+", " ") %>%
    str_trim()
  
  # Extract pagination links (next page button)
  # Second class because first is previous page here get both
  pagination_links <- webpage %>%
    html_nodes(".pagination.controls__pagination a.button.button--xsmall") %>%
    html_attr("href")
  
  # Choose the second button (Next Page)
  next_page_url <- if (length(pagination_links) >= 2) {
    pagination_links[2]
  } else {
    NA
  }
  
  return(list(quotes = quotes_clean, next_page_url = next_page_url))
}