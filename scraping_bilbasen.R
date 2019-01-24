# Importing libraries for the scraping
library(tidyverse)
library(rvest)
library(rebus)
library(lubridate)

#### Scrape setup - input and output for scrape function ####

# Landing page - entry page 
url_audi_a4 <- 'https://www.bilbasen.dk/brugt/bil/audi/a4'
url_vw_passat <- 'https://www.bilbasen.dk/brugt/bil/vw/passat'
url_vw_golfVII <- 'https://www.bilbasen.dk/brugt/bil/VW/Golf%20VII'

# Create list of input
input_vec <- list(url_audi_a4, url_vw_passat, url_vw_golfVII)
# Create list of corresponding output names
output_vec <- list("audi_a4", "vw_passat", "vw_golfVII")



#### Function to scrape bilbasen ####
scrape_bilbasen <- function(url, output_name){ 
  
  ## Automate identification of last page: ##
  # Iteratively move through every page in a while loop until the last page has been found.
  # The loop searches for the "page_no page_no page_no page_no page_no next_page" sequence
  # (e.g. "12345>") found at the bottom of every page in Bilbasen.
  # The while loop has two limitations: 
  #   1. There has to be at least 6 pages of cars (in the 32 cars per page default setup)
  #   2. There cannot be more than 
  
  # Initiate i
  i <- 1
  
  # Execute while loop to find last page
  while (i > 0) {
    
    # Define url to scan
    url_i <- str_c(url, '?page=', i)
    # Read url into a variable
    html <- read_html(url_i)
    
    text <- html %>%
      # Use relevant tag for finding the number of pages links in the bottom of the page
      html_nodes("ul") %>%
      # Get the text of all occurances of "ul"
      html_text()  
    
    # Search for and extract a sequence of numbers ending with a ">" - e.g. "12345>"
    num_seq <- text[str_detect(text, "\\d+>")] %>% str_trim()
    print(str_c("Looking at page ",i, " of ", output_name))
    
    # Check that the ">" detection runs as it should - ">" exists and no more than 1 result is
    # generated in num_seq
    if(i == 1 & (length(num_seq) != 1)) {
      stop("Number of pages is less than 6 or unable to detect '>' for another reason. 
          Please run program manually: Skip page detection and input last page
          manually.")
    }
    
    # If the string contains ">" then next iteration
    if (length(num_seq) == 1) {
      i <- i + 1
    } else {
      # If the text does not contain ">" (meaning that the last page has been found)
      # then set i as no_pages
      no_pages <- i
      # Exit while loop
      break
    }
    
    # Force stop the ">" detection if number of pages exceeds 99 - it seems unrealistic that is
    # the case
    if(i == 97) {
      # If there is more than 99 pages then set no_pages to 99 and stop loop
      no_pages <- 99
      stop("Number of pages exceeds 99 - is this really true?")
    }
  }
  print(str_c("The total number of pages for a ", output_name, " is: ", no_pages))
  
  
  # Generate a list of all pages' url's
  list_of_pages <- str_c(url, '?page=',1:no_pages)
  
  ### Extracting the data for one page ###
  
  # Model:
  get_model <- function(html){
    html %>%
      # The relevant tag
      html_nodes('.darkLink') %>%
      html_text() %>%
      # Trim additional white space
      str_trim() %>%
      # Convert the list to a vector
      unlist()
  }
  
  # Description: - having issues - may be implemented later
  # the tag is: ".expandable-box"
  
  # Place:
  get_place <- function(html){
    html %>%
      # The relevant tag
      html_nodes('.listing-region') %>%
      html_text() %>%
      # Trim additional white space
      str_trim() %>%
      # Convert the list to a vector
      unlist()
  }
  
  # Kilometers per liter:
  get_kml <- function(html){
    html %>%
      # The relevant tag
      html_nodes('.variableDataColumn') %>%
      html_text() %>%
      # Trim additional white space
      str_trim() %>%
      # Convert the list to a vector
      unlist()
  }
  
  # Kilometers driven:
  get_km <- function(html){
    html %>%
      # The relevant tag
      html_nodes('.listing-data:nth-child(3)') %>%
      html_text() %>%
      # Trim additional white space
      str_trim() %>%
      # Convert the list to a vector
      unlist()
  }
  
  # Model year:
  get_year <- function(html){
    html %>%
      # The relevant tag
      html_nodes('.listing-data:nth-child(4)') %>%
      html_text() %>%
      # Trim additional white space
      str_trim() %>%
      # Convert the list to a vector
      unlist()
  }
  
  # Price:
  get_price <- function(html){
    html %>%
      # The relevant tag
      html_nodes('.listing-price') %>%
      html_text() %>%
      # Trim additional white space
      str_trim() %>%
      # Convert the list to a vector
      unlist()
  }
  
  # Model:
  get_label <- function(html){
    html %>%
      # The relevant tag
      html_nodes('.listing_label') %>%
      html_text() %>%
      # Trim additional white space
      str_trim() %>%
      # Convert the list to a vector
      unlist()
  }
  
  # Extracting the data - add different car models later as an extra input parameter
  get_data_table <- function(html){
    
    #Extract the data from the HTML into vectors
    model_ext <- get_model(html)
    #  description_ext <- get_description(html)
    place_ext <- get_place(html)
    kml_ext <- get_kml(html)
    km_ext <- get_km(html)
    year_ext <- get_year(html)
    price_ext <- get_price(html)
    
    # Combine into a tibble
    combined_data <- tibble(model = model_ext,
                            # descript = description_ext,
                            place = place_ext,
                            kml = kml_ext,
                            km = km_ext,
                            year = year_ext,
                            price = price_ext)
  }
  
  # Extract the data using a function - here additional car model will be added
  get_data_from_url <- function(url){
    html <- read_html(url)
    get_data_table(html) # this is where additional models can be added
  }
  
  # Apply the extraction and bind each page into one table that is exported as a tsv file
  output_df <- list_of_pages %>%
    # Apply function on all pages
    map(get_data_from_url) %>%
    # Combine the individual tibble into one
    bind_rows()
  
  # Print the output to the terminal
  print(output_df)  
  
  # Export as tsv
  write_tsv(output_df, paste('Documents/DataScience/Datasets/bilbasen_', 
                             output_name, "_", Sys.Date(), '.tsv', sep = ""))
}


#### Function call ####
# Call scrape_bilbasen on all inputs and outputs
for (i in 1:length(input_vec)) {
  scrape_bilbasen(url = input_vec[i], output_name = output_vec[i])
}


