####### Scrape and Clean Bilbasen Data #######

# Import needed libraries
library(tidyverse) # TidyVerse functionality
library(rvest) # Scraping tool
library(rebus) 
library(lubridate)

#### Scrape setup - input and output for scrape function ####

# Landing page - entry page 
url_audi_a4 <- 'https://www.bilbasen.dk/brugt/bil/audi/a4'
url_audi_a6 <- 'https://www.bilbasen.dk/brugt/bil/audi/a6'
url_vw_passat <- 'https://www.bilbasen.dk/brugt/bil/vw/passat'
url_vw_golfVI <- 'https://www.bilbasen.dk/brugt/bil/VW/Golf%20VI'
url_vw_golfVII <- 'https://www.bilbasen.dk/brugt/bil/VW/Golf%20VII'
url_skoda_octavia <- 'https://www.bilbasen.dk/brugt/bil/skoda/octavia'
url_skoda_superb <- 'https://www.bilbasen.dk/brugt/bil/skoda/superb'

# Create list of input
input_vec <- list(url_audi_a4, url_audi_a6, url_vw_passat, url_vw_golfVI, url_vw_golfVII, url_skoda_octavia, url_skoda_superb)
# Create list of inputs to corresponding output names
output_vec <- list("audi_a4", "audi_a6", "vw_passat", "vw_golfvi", "vw_golfvii", "skoda_octavia", "skoda_superb")


#### Function to scrape bilbasen ####
scrape_bilbasen <- function(url, output_name){ 
  
  ## Automate identification of last page: ##
  # Iteratively move through every page in a while loop until the last page has been found.
  # The loop searches for the "page_no page_no page_no page_no page_no next_page" sequence
  # (e.g. "12345>") found at the bottom of every page in Bilbasen.
  # The while loop has two limitations: 
  #   1. There has to be at least 6 pages of cars (in the 32 cars per page default setup)
  #   2. There cannot be more than 99 pages to scrape - a safeguard against an infinite loop
  
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

#### Cleaning the datasets ####

# Import datasets
audi_a4_raw <- read_tsv(paste('Documents/DataScience/Datasets/bilbasen_audi_a4_', Sys.Date(), '.tsv', sep = ""))
audi_a6_raw <- read_tsv(paste('Documents/DataScience/Datasets/bilbasen_audi_a6_', Sys.Date(), '.tsv', sep = ""))
vw_passat_raw <- read_tsv(paste('Documents/DataScience/Datasets/bilbasen_vw_passat_', Sys.Date(), '.tsv', sep = ""))
vw_golfVI_raw <- read_tsv(paste('Documents/DataScience/Datasets/bilbasen_vw_golfVI_', Sys.Date(), '.tsv', sep = ""))
vw_golfVII_raw <- read_tsv(paste('Documents/DataScience/Datasets/bilbasen_vw_golfVII_', Sys.Date(), '.tsv', sep = ""))
skoda_octavia_raw <- read_tsv(paste('Documents/DataScience/Datasets/bilbasen_skoda_octavia_', Sys.Date(), '.tsv', sep = ""))
skoda_superb_raw <- read_tsv(paste('Documents/DataScience/Datasets/bilbasen_skoda_superb_', Sys.Date(), '.tsv', sep = ""))

# Set up regex function used to create variables from string input
extract_element <- function(string, input_regex) {
  # Function embedded in next function. Used to extract element from string if this is present.
  extraction <- gregexpr(input_regex, string)
  unlist(regmatches(string, extraction))
}
extract_variable <- function(string, input_regex) {
  # Checks if element searched for is present in the string. If so then it is extracted else a
  # placeholder values is set.
  ans <- ifelse(grepl(input_regex, string), 
                extract_element(string, input_regex), 
                "not_found_in_string")
  return(ans)
}


### Audi A4 ###

# Create variables from raw model variable extraction
audi_a4_raw1 <- audi_a4_raw
audi_a4_raw1$brand <- unlist(lapply(audi_a4_raw$model, extract_variable, "(Audi)"))
audi_a4_raw1$model1 <- unlist(lapply(audi_a4_raw$model, extract_variable, "(A4)"))
audi_a4_raw1$eng_size <- unlist(lapply(audi_a4_raw$model, extract_variable, "([0-9],[0-9])"))
audi_a4_raw1$eng_type <- unlist(lapply(audi_a4_raw$model, extract_variable, "(TDi|TFSi|FSi|T)"))
audi_a4_raw1$HP <- unlist(lapply(audi_a4_raw$model, extract_variable, "( [0-9]+ )"))
audi_a4_raw1$type <- unlist(lapply(audi_a4_raw$model, extract_variable, "(Avant)"))
audi_a4_raw1$transm <- unlist(lapply(audi_a4_raw$model, extract_variable, "(S-tr|S-tr.|Multitr.|Tiptr.)"))

# Clean eng_size variable
audi_a4_raw1$eng_size <- audi_a4_raw1$eng_size %>%
  str_replace(",", ".") %>%
  as.numeric()
audi_a4_raw1$eng_size <- round(audi_a4_raw1$eng_size, 1)

# Clean eng_type variable - it is assumed that only the values TDi, TSi, GTE or GTi are present
audi_a4_raw1 <- mutate(audi_a4_raw1, eng_type = case_when(eng_type == "TDi" ~ "Diesel",
                                                              eng_type == "FSi" ~ "Gasoline",
                                                              eng_type == "TFSi" ~ "Gasoline",
                                                              eng_type == "T" ~ "Gasoline"))
audi_a4_raw1$eng_type <- as.factor(audi_a4_raw1$eng_type)

# Clean HP variable
audi_a4_raw1$HP <- as.numeric(audi_a4_raw1$HP)

# Encode type variable - if any of the station car 
# keywords are not mentioned the car is assumed to be a coupe
audi_a4_raw1$type <- as.factor(ifelse(audi_a4_raw1$type == "Avant", "station",  "coupe")) 

# Encode transmission variable - if type is not DSG then the car is 
# assumed to be manual
audi_transm <- c("S-tr", "S-tr.", "Multitr.", "Tiptr.")
audi_a4_raw1$transm <- as.factor(ifelse(audi_a4_raw1$transm %in% audi_transm, "auto",  "manual"))

# Clean the kml column
audi_a4_raw1 <- separate(audi_a4_raw1, kml, c("kml", "unit"), sep = " ")
audi_a4_raw1 <- select(audi_a4_raw1, -unit)
audi_a4_raw1$kml <- audi_a4_raw1$kml %>%
  str_replace(",", ".") %>%
  as.numeric(audi_a4_raw1$kml)

# Clean the km column
if (typeof(audi_a4_raw1$km) == "character") {
  audi_a4_raw1$km <- as.numeric(audi_a4_raw1$km) * 1000
} else {
  audi_a4_raw1$km <- audi_a4_raw1$km * 1000
}

# Clean place
audi_a4_raw1$place <- gsub('"', "", audi_a4_raw1$place, fixed = T)
audi_a4_raw1$place <- as.factor(audi_a4_raw1$place)

# Clean price
audi_a4_raw1 <- audi_a4_raw1 %>%
  separate(price, c("price", "unit"), sep = " ") %>%
  select(-unit)
audi_a4_raw1$price <- gsub(".", "", audi_a4_raw1$price, fixed = T)
audi_a4_raw1$price <- as.numeric(audi_a4_raw1$price)

# Extracting NAs from the dataset for counting / QC
audi_a4_na <- audi_a4_raw1[rowSums(is.na(audi_a4_raw1)) > 0, ]

# Removing all rows with a price less than 10.000kr
audi_a4_raw1 <- filter(audi_a4_raw1, price > 10000)

# Setting final vw_passet dataset and removing any rows with NA's
audi_a4 <- audi_a4_raw1 %>% 
  select(brand, model1, eng_size, eng_type, HP, type, transm, place, kml, km, year, price) %>% 
  na.omit()

names(audi_a4)[names(audi_a4) == "model1"] <- "model"



### Audi A6 ###

# Create variables from raw model variable extraction
audi_a6_raw1 <- audi_a6_raw
audi_a6_raw1$brand <- unlist(lapply(audi_a6_raw$model, extract_variable, "(Audi)"))
audi_a6_raw1$model1 <- unlist(lapply(audi_a6_raw$model, extract_variable, "(A6)"))
audi_a6_raw1$eng_size <- unlist(lapply(audi_a6_raw$model, extract_variable, "([0-9],[0-9])"))
audi_a6_raw1$eng_type <- unlist(lapply(audi_a6_raw$model, extract_variable, "(TDi|TFSi|FSi|T)"))
audi_a6_raw1$HP <- unlist(lapply(audi_a6_raw$model, extract_variable, "( [0-9]+ )"))
audi_a6_raw1$type <- unlist(lapply(audi_a6_raw$model, extract_variable, "(Avant)"))
audi_a6_raw1$transm <- unlist(lapply(audi_a6_raw$model, extract_variable, "(S-tr|S-tr.|Multitr.|Tiptr.)"))

# Clean eng_size variable
audi_a6_raw1$eng_size <- audi_a6_raw1$eng_size %>%
  str_replace(",", ".") %>%
  as.numeric()
audi_a6_raw1$eng_size <- round(audi_a6_raw1$eng_size, 1)

# Clean eng_type variable - it is assumed that only the values TDi, TSi, GTE or GTi are present
audi_a6_raw1 <- mutate(audi_a6_raw1, eng_type = case_when(eng_type == "TDi" ~ "Diesel",
                                                          eng_type == "FSi" ~ "Gasoline",
                                                          eng_type == "TFSi" ~ "Gasoline",
                                                          eng_type == "T" ~ "Gasoline"))
audi_a6_raw1$eng_type <- as.factor(audi_a6_raw1$eng_type)

# Clean HP variable
audi_a6_raw1$HP <- as.numeric(audi_a6_raw1$HP)

# Encode type variable - if any of the station car 
# keywords are not mentioned the car is assumed to be a coupe
audi_a6_raw1$type <- as.factor(ifelse(audi_a6_raw1$type == "Avant", "station",  "coupe")) 

# Encode transmission variable - if type is not DSG then the car is 
# assumed to be manual
audi_transm <- c("S-tr", "S-tr.", "Multitr.", "Tiptr.")
audi_a6_raw1$transm <- as.factor(ifelse(audi_a6_raw1$transm %in% audi_transm, "auto",  "manual"))

# Clean the kml column
audi_a6_raw1 <- separate(audi_a6_raw1, kml, c("kml", "unit"), sep = " ")
audi_a6_raw1 <- select(audi_a6_raw1, -unit)
audi_a6_raw1$kml <- audi_a6_raw1$kml %>%
  str_replace(",", ".") %>%
  as.numeric(audi_a6_raw1$kml)

# Clean the km column
if (typeof(audi_a6_raw1$km) == "character") {
  audi_a6_raw1$km <- as.numeric(audi_a6_raw1$km) * 1000
} else {
  audi_a6_raw1$km <- audi_a6_raw1$km * 1000
}

# Clean place
audi_a6_raw1$place <- gsub('"', "", audi_a6_raw1$place, fixed = T)
audi_a6_raw1$place <- as.factor(audi_a6_raw1$place)

# Clean price
audi_a6_raw1 <- audi_a6_raw1 %>%
  separate(price, c("price", "unit"), sep = " ") %>%
  select(-unit)
audi_a6_raw1$price <- gsub(".", "", audi_a6_raw1$price, fixed = T)
audi_a6_raw1$price <- as.numeric(audi_a6_raw1$price)

# Extracting NAs from the dataset for counting / QC
audi_a6_na <- audi_a6_raw1[rowSums(is.na(audi_a6_raw1)) > 0, ]

# Removing all rows with a price less than 10.000kr
audi_a6_raw1 <- filter(audi_a6_raw1, price > 10000)

# Setting final vw_passet dataset and removing any rows with NA's
audi_a6 <- audi_a6_raw1 %>% 
  select(brand, model1, eng_size, eng_type, HP, type, transm, place, kml, km, year, price) %>% 
  na.omit()

names(audi_a6)[names(audi_a6) == "model1"] <- "model"


### VW Passat ###

# Create variables from raw model variable extraction
vw_passat_raw1 <- vw_passat_raw
vw_passat_raw1$brand <- unlist(lapply(vw_passat_raw$model, extract_variable, "(VW)"))
vw_passat_raw1$model1 <- unlist(lapply(vw_passat_raw$model, extract_variable, "(Passat)"))
vw_passat_raw1$eng_size <- unlist(lapply(vw_passat_raw$model, extract_variable, "([0-9],[0-9])"))
vw_passat_raw1$eng_type <- unlist(lapply(vw_passat_raw$model, extract_variable, "(TDi|TSi|GTE|GTi)"))
vw_passat_raw1$HP <- unlist(lapply(vw_passat_raw$model, extract_variable, "( [0-9]+ )"))
vw_passat_raw1$type <- unlist(lapply(vw_passat_raw$model, extract_variable, "(Variant|Vari|Vari.)"))
vw_passat_raw1$transm <- unlist(lapply(vw_passat_raw$model, extract_variable, "(DSG)"))

# Clean eng_size variable
vw_passat_raw1$eng_size <- vw_passat_raw1$eng_size %>%
  str_replace(",", ".") %>%
  as.numeric()
vw_passat_raw1$eng_size <- round(vw_passat_raw1$eng_size, 1)

# Clean eng_type variable - it is assumed that only the values TDi, TSi, GTE or GTi are present
vw_passat_raw1 <- mutate(vw_passat_raw1, eng_type = case_when(eng_type == "TDi" ~ "Diesel",
                                                              eng_type == "TSi" ~ "Gasoline",
                                                              eng_type == "GTi" ~ "Gasoline",
                                                              eng_type == "GTE" ~ "Hybrid"))
vw_passat_raw1$eng_type <- as.factor(vw_passat_raw1$eng_type)

# Clean HP variable
vw_passat_raw1$HP <- as.numeric(vw_passat_raw1$HP)

# Encode type variable - if any of the station car 
# keywords are not mentioned the car is assumed to be a coupe
stcar <- c("Variant", "Vari.", "Vari")
vw_passat_raw1$type <- as.factor(ifelse(vw_passat_raw1$type %in% stcar, "station",  "coupe")) 

# Encode transmission variable - if type is not DSG then the car is 
# assumed to be manual
vw_passat_raw1$transm <- as.factor(ifelse(vw_passat_raw1$transm == "DSG", "auto",  "manual"))

# Clean the kml column
vw_passat_raw1 <- separate(vw_passat_raw1, kml, c("kml", "unit"), sep = " ")
vw_passat_raw1 <- select(vw_passat_raw1, -unit)
vw_passat_raw1$kml <- vw_passat_raw1$kml %>%
  str_replace(",", ".") %>%
  as.numeric(vw_passat_raw1$kml)

# Clean the km column
if (typeof(vw_passat_raw1$km) == "character") {
  vw_passat_raw1$km <- as.numeric(vw_passat_raw1$km) * 1000
} else {
  vw_passat_raw1$km <- vw_passat_raw1$km * 1000
}

# Clean place
vw_passat_raw1$place <- gsub('"', "", vw_passat_raw1$place, fixed = T)
vw_passat_raw1$place <- as.factor(vw_passat_raw1$place)

# Clean price
vw_passat_raw1 <- vw_passat_raw1 %>%
  separate(price, c("price", "unit"), sep = " ") %>%
  select(-unit)
vw_passat_raw1$price <- gsub(".", "", vw_passat_raw1$price, fixed = T)
vw_passat_raw1$price <- as.numeric(vw_passat_raw1$price)

# Extracting NAs from the dataset for counting / QC
vw_passat_na <- vw_passat_raw1[rowSums(is.na(vw_passat_raw1)) > 0, ]

# Removing all rows with a price less than 10.000kr
vw_passat_raw1 <- filter(vw_passat_raw1, price > 10000)

# Setting final vw_passet dataset and removing any rows with NA's
vw_passat <- vw_passat_raw1 %>% 
  select(brand, model1, eng_size, eng_type, HP, type, transm, place, kml, km, year, price) %>% 
  na.omit()

names(vw_passat)[names(vw_passat) == "model1"] <- "model"



### VW Golf VI ###

# Create variables from raw model variable extraction
vw_golfVI_raw1 <- vw_golfVI_raw
vw_golfVI_raw1$brand <- unlist(lapply(vw_golfVI_raw$model, extract_variable, "(VW)"))
vw_golfVI_raw1$model1 <- unlist(lapply(vw_golfVI_raw$model, extract_variable, "(Golf VI)"))
vw_golfVI_raw1$eng_size <- unlist(lapply(vw_golfVI_raw$model, extract_variable, "([0-9],[0-9])"))
vw_golfVI_raw1$eng_type <- unlist(lapply(vw_golfVI_raw$model, extract_variable, "(TDi|TSi|GTE|GTi)"))
vw_golfVI_raw1$HP <- unlist(lapply(vw_golfVI_raw$model, extract_variable, "( [0-9]+ )"))
vw_golfVI_raw1$type <- unlist(lapply(vw_golfVI_raw$model, extract_variable, "(Variant|Vari|Vari.)"))
vw_golfVI_raw1$transm <- unlist(lapply(vw_golfVI_raw$model, extract_variable, "(DSG)"))

# Clean model variable
vw_golfVI_raw1$model1 <- str_replace_all(vw_golfVI_raw1$model1, space(), "")

# Clean eng_size variable
vw_golfVI_raw1$eng_size <- vw_golfVI_raw1$eng_size %>%
  str_replace(",", ".") %>%
  as.numeric()
vw_golfVI_raw1$eng_size <- round(vw_golfVI_raw1$eng_size, 1)

# Clean eng_type variable - it is assumed that only the values TDi, TSi, GTE or GTi are present
vw_golfVI_raw1 <- mutate(vw_golfVI_raw1, eng_type = case_when(eng_type == "TDi" ~ "Diesel",
                                                              eng_type == "TSi" ~ "Gasoline",
                                                              eng_type == "GTi" ~ "Gasoline",
                                                              eng_type == "GTE" ~ "Hybrid"))
vw_golfVI_raw1$eng_type <- as.factor(vw_golfVI_raw1$eng_type)

# Clean HP variable
vw_golfVI_raw1$HP <- as.numeric(vw_golfVI_raw1$HP)

# Encode type variable - if any of the station car 
# keywords are not mentioned the car is assumed to be a coupe
stcar <- c("Variant", "Vari.", "Vari")
vw_golfVI_raw1$type <- as.factor(ifelse(vw_golfVI_raw1$type %in% stcar, "station",  "coupe")) 

# Encode transmission variable - if type is not DSG then the car is 
# assumed to be manual
vw_golfVI_raw1$transm <- as.factor(ifelse(vw_golfVI_raw1$transm == "DSG", "auto",  "manual"))

# Clean the kml column
vw_golfVI_raw1 <- separate(vw_golfVI_raw1, kml, c("kml", "unit"), sep = " ")
vw_golfVI_raw1 <- select(vw_golfVI_raw1, -unit)
vw_golfVI_raw1$kml <- vw_golfVI_raw1$kml %>%
  str_replace(",", ".") %>%
  as.numeric(vw_golfVI_raw1$kml)

# Clean the km column
if (typeof(vw_golfVI_raw1$km) == "character") {
  vw_golfVI_raw1$km <- as.numeric(vw_golfVI_raw1$km) * 1000
} else {
  vw_golfVI_raw1$km <- vw_golfVI_raw1$km * 1000
}

# Clean place
vw_golfVI_raw1$place <- gsub('"', "", vw_golfVI_raw1$place, fixed = T)
vw_golfVI_raw1$place <- as.factor(vw_golfVI_raw1$place)

# Clean price
vw_golfVI_raw1 <- vw_golfVI_raw1 %>%
  separate(price, c("price", "unit"), sep = " ") %>%
  select(-unit)
vw_golfVI_raw1$price <- gsub(".", "", vw_golfVI_raw1$price, fixed = T)
vw_golfVI_raw1$price <- as.numeric(vw_golfVI_raw1$price)

# Extracting NAs from the dataset for counting / QC
vw_golfVI_na <- vw_golfVI_raw1[rowSums(is.na(vw_golfVI_raw1)) > 0, ]

# Removing all rows with a price less than 10.000kr
vw_golfVI_raw1 <- filter(vw_golfVI_raw1, price > 10000)

# Setting final vw_passet dataset and removing any rows with NA's
vw_golfVI <- vw_golfVI_raw1 %>% 
  select(brand, model1, eng_size, eng_type, HP, type, transm, place, kml, km, year, price) %>% 
  na.omit()

names(vw_golfVI)[names(vw_golfVI) == "model1"] <- "model"


### VW Golf VII ###

# Create variables from raw model variable extraction
vw_golfVII_raw1 <- vw_golfVII_raw
vw_golfVII_raw1$brand <- unlist(lapply(vw_golfVII_raw$model, extract_variable, "(VW)"))
vw_golfVII_raw1$model1 <- unlist(lapply(vw_golfVII_raw$model, extract_variable, "(Golf VII)"))
vw_golfVII_raw1$eng_size <- unlist(lapply(vw_golfVII_raw$model, extract_variable, "([0-9],[0-9])"))
vw_golfVII_raw1$eng_type <- unlist(lapply(vw_golfVII_raw$model, extract_variable, "(TDi|TSi|GTE|GTi)"))
vw_golfVII_raw1$HP <- unlist(lapply(vw_golfVII_raw$model, extract_variable, "( [0-9]+ )"))
vw_golfVII_raw1$type <- unlist(lapply(vw_golfVII_raw$model, extract_variable, "(Variant|Vari|Vari.)"))
vw_golfVII_raw1$transm <- unlist(lapply(vw_golfVII_raw$model, extract_variable, "(DSG)"))

# Clean model variable
vw_golfVII_raw1$model1 <- str_replace_all(vw_golfVII_raw1$model1, space(), "")

# Clean eng_size variable
vw_golfVII_raw1$eng_size <- vw_golfVII_raw1$eng_size %>%
  str_replace(",", ".") %>%
  as.numeric()
vw_golfVII_raw1$eng_size <- round(vw_golfVII_raw1$eng_size, 1)

# Clean eng_type variable - it is assumed that only the values TDi, TSi, GTE or GTi are present
vw_golfVII_raw1 <- mutate(vw_golfVII_raw1, eng_type = case_when(eng_type == "TDi" ~ "Diesel",
                                                              eng_type == "TSi" ~ "Gasoline",
                                                              eng_type == "GTi" ~ "Gasoline",
                                                              eng_type == "GTE" ~ "Hybrid"))
vw_golfVII_raw1$eng_type <- as.factor(vw_golfVII_raw1$eng_type)

# Clean HP variable
vw_golfVII_raw1$HP <- as.numeric(vw_golfVII_raw1$HP)

# Encode type variable - if any of the station car 
# keywords are not mentioned the car is assumed to be a coupe
stcar <- c("Variant", "Vari.", "Vari")
vw_golfVII_raw1$type <- as.factor(ifelse(vw_golfVII_raw1$type %in% stcar, "station",  "coupe")) 

# Encode transmission variable - if type is not DSG then the car is 
# assumed to be manual
vw_golfVII_raw1$transm <- as.factor(ifelse(vw_golfVII_raw1$transm == "DSG", "auto",  "manual"))

# Clean the kml column
vw_golfVII_raw1 <- separate(vw_golfVII_raw1, kml, c("kml", "unit"), sep = " ")
vw_golfVII_raw1 <- select(vw_golfVII_raw1, -unit)
vw_golfVII_raw1$kml <- vw_golfVII_raw1$kml %>%
  str_replace(",", ".") %>%
  as.numeric(vw_golfVII_raw1$kml)

# Clean the km column
if (typeof(vw_golfVII_raw1$km) == "character") {
  vw_golfVII_raw1$km <- as.numeric(vw_golfVII_raw1$km) * 1000
} else {
  vw_golfVII_raw1$km <- vw_golfVII_raw1$km * 1000
}

# Clean place
vw_golfVII_raw1$place <- gsub('"', "", vw_golfVII_raw1$place, fixed = T)
vw_golfVII_raw1$place <- as.factor(vw_golfVII_raw1$place)

# Clean price
vw_golfVII_raw1 <- vw_golfVII_raw1 %>%
  separate(price, c("price", "unit"), sep = " ") %>%
  select(-unit)
vw_golfVII_raw1$price <- gsub(".", "", vw_golfVII_raw1$price, fixed = T)
vw_golfVII_raw1$price <- as.numeric(vw_golfVII_raw1$price)

# Extracting NAs from the dataset for counting / QC
vw_golfVII_na <- vw_golfVII_raw1[rowSums(is.na(vw_golfVII_raw1)) > 0, ]

# Removing all rows with a price less than 10.000kr
vw_golfVII_raw1 <- filter(vw_golfVII_raw1, price > 10000)

# Setting final vw_passet dataset and removing any rows with NA's
vw_golfVII <- vw_golfVII_raw1 %>% 
  select(brand, model1, eng_size, eng_type, HP, type, transm, place, kml, km, year, price) %>% 
  na.omit()

names(vw_golfVII)[names(vw_golfVII) == "model1"] <- "model"


### Skoda Octavia ###

# Create variables from raw model variable extraction
skoda_octavia_raw1 <- skoda_octavia_raw
skoda_octavia_raw1$brand <- unlist(lapply(skoda_octavia_raw$model, extract_variable, "(Skoda)"))
skoda_octavia_raw1$model1 <- unlist(lapply(skoda_octavia_raw$model, extract_variable, "(Octavia)"))
skoda_octavia_raw1$eng_size <- unlist(lapply(skoda_octavia_raw$model, extract_variable, "([0-9],[0-9])"))
skoda_octavia_raw1$eng_type <- unlist(lapply(skoda_octavia_raw$model, extract_variable, "(TDi|TSi|FSi|TFSi)"))
skoda_octavia_raw1$HP <- unlist(lapply(skoda_octavia_raw$model, extract_variable, "( [0-9]+ )"))
skoda_octavia_raw1$type <- unlist(lapply(skoda_octavia_raw$model, extract_variable, "(Combi)"))
skoda_octavia_raw1$transm <- unlist(lapply(skoda_octavia_raw$model, extract_variable, "(DSG)"))

# Clean eng_size variable
skoda_octavia_raw1$eng_size <- skoda_octavia_raw1$eng_size %>%
  str_replace(",", ".") %>%
  as.numeric()
skoda_octavia_raw1$eng_size <- round(skoda_octavia_raw1$eng_size, 1)

# Clean eng_type variable - it is assumed that only the values TDi, TSi, GTE or GTi are present
skoda_octavia_raw1 <- mutate(skoda_octavia_raw1, eng_type = case_when(eng_type == "TDi" ~ "Diesel",
                                                                eng_type == "TSi" ~ "Gasoline",
                                                                eng_type == "FSi" ~ "Gasoline",
                                                                eng_type == "TFSi" ~ "Gasoline"))
skoda_octavia_raw1$eng_type <- as.factor(skoda_octavia_raw1$eng_type)

# Clean HP variable
skoda_octavia_raw1$HP <- as.numeric(skoda_octavia_raw1$HP)

# Encode type variable - if any of the station car 
# keywords are not mentioned the car is assumed to be a coupe
skoda_octavia_raw1$type <- as.factor(ifelse(skoda_octavia_raw1$type == "Combi", "station",  "coupe")) 

# Encode transmission variable - if type is not DSG then the car is 
# assumed to be manual
skoda_octavia_raw1$transm <- as.factor(ifelse(skoda_octavia_raw1$transm == "DSG", "auto",  "manual"))

# Clean the kml column
skoda_octavia_raw1 <- separate(skoda_octavia_raw1, kml, c("kml", "unit"), sep = " ")
skoda_octavia_raw1 <- select(skoda_octavia_raw1, -unit)
skoda_octavia_raw1$kml <- skoda_octavia_raw1$kml %>%
  str_replace(",", ".") %>%
  as.numeric(skoda_octavia_raw1$kml)

# Clean the km column
if (typeof(skoda_octavia_raw1$km) == "character") {
  skoda_octavia_raw1$km <- as.numeric(skoda_octavia_raw1$km) * 1000
} else {
  skoda_octavia_raw1$km <- skoda_octavia_raw1$km * 1000
}

# Clean place
skoda_octavia_raw1$place <- gsub('"', "", skoda_octavia_raw1$place, fixed = T)
skoda_octavia_raw1$place <- as.factor(skoda_octavia_raw1$place)

# Clean price
skoda_octavia_raw1 <- skoda_octavia_raw1 %>%
  separate(price, c("price", "unit"), sep = " ") %>%
  select(-unit)
skoda_octavia_raw1$price <- gsub(".", "", skoda_octavia_raw1$price, fixed = T)
skoda_octavia_raw1$price <- as.numeric(skoda_octavia_raw1$price)

# Extracting NAs from the dataset for counting / QC
skoda_octavia_na <- skoda_octavia_raw1[rowSums(is.na(skoda_octavia_raw1)) > 0, ]

# Removing all rows with a price less than 10.000kr
skoda_octavia_raw1 <- filter(skoda_octavia_raw1, price > 10000)

# Setting final vw_passet dataset and removing any rows with NA's
skoda_octavia <- skoda_octavia_raw1 %>% 
  select(brand, model1, eng_size, eng_type, HP, type, transm, place, kml, km, year, price) %>% 
  na.omit()

names(skoda_octavia)[names(skoda_octavia) == "model1"] <- "model"


### Skoda Superb ###

# Create variables from raw model variable extraction
skoda_superb_raw1 <- skoda_superb_raw
skoda_superb_raw1$brand <- unlist(lapply(skoda_superb_raw$model, extract_variable, "(Skoda)"))
skoda_superb_raw1$model1 <- unlist(lapply(skoda_superb_raw$model, extract_variable, "(Superb)"))
skoda_superb_raw1$eng_size <- unlist(lapply(skoda_superb_raw$model, extract_variable, "([0-9],[0-9])"))
skoda_superb_raw1$eng_type <- unlist(lapply(skoda_superb_raw$model, extract_variable, "(TDi|TSi|FSi|TFSi)"))
skoda_superb_raw1$HP <- unlist(lapply(skoda_superb_raw$model, extract_variable, "( [0-9]+ )"))
skoda_superb_raw1$type <- unlist(lapply(skoda_superb_raw$model, extract_variable, "(Combi)"))
skoda_superb_raw1$transm <- unlist(lapply(skoda_superb_raw$model, extract_variable, "(DSG)"))

# Clean eng_size variable
skoda_superb_raw1$eng_size <- skoda_superb_raw1$eng_size %>%
  str_replace(",", ".") %>%
  as.numeric()
skoda_superb_raw1$eng_size <- round(skoda_superb_raw1$eng_size, 1)

# Clean eng_type variable - it is assumed that only the values TDi, TSi, GTE or GTi are present
skoda_superb_raw1 <- mutate(skoda_superb_raw1, eng_type = case_when(eng_type == "TDi" ~ "Diesel",
                                                                      eng_type == "TSi" ~ "Gasoline",
                                                                      eng_type == "FSi" ~ "Gasoline",
                                                                      eng_type == "TFSi" ~ "Gasoline"))
skoda_superb_raw1$eng_type <- as.factor(skoda_superb_raw1$eng_type)

# Clean HP variable
skoda_superb_raw1$HP <- as.numeric(skoda_superb_raw1$HP)

# Encode type variable - if any of the station car 
# keywords are not mentioned the car is assumed to be a coupe
skoda_superb_raw1$type <- as.factor(ifelse(skoda_superb_raw1$type == "Combi", "station",  "coupe")) 

# Encode transmission variable - if type is not DSG then the car is 
# assumed to be manual
skoda_superb_raw1$transm <- as.factor(ifelse(skoda_superb_raw1$transm == "DSG", "auto",  "manual"))

# Clean the kml column
skoda_superb_raw1 <- separate(skoda_superb_raw1, kml, c("kml", "unit"), sep = " ")
skoda_superb_raw1 <- select(skoda_superb_raw1, -unit)
skoda_superb_raw1$kml <- skoda_superb_raw1$kml %>%
  str_replace(",", ".") %>%
  as.numeric(skoda_superb_raw1$kml)

# Clean the km column
if (typeof(skoda_superb_raw1$km) == "character") {
  skoda_superb_raw1$km <- as.numeric(skoda_superb_raw1$km) * 1000
} else {
  skoda_superb_raw1$km <- skoda_superb_raw1$km * 1000
}

# Clean place
skoda_superb_raw1$place <- gsub('"', "", skoda_superb_raw1$place, fixed = T)
skoda_superb_raw1$place <- as.factor(skoda_superb_raw1$place)

# Clean price
skoda_superb_raw1 <- skoda_superb_raw1 %>%
  separate(price, c("price", "unit"), sep = " ") %>%
  select(-unit)
skoda_superb_raw1$price <- gsub(".", "", skoda_superb_raw1$price, fixed = T)
skoda_superb_raw1$price <- as.numeric(skoda_superb_raw1$price)

# Extracting NAs from the dataset for counting / QC
skoda_superb_na <- skoda_superb_raw1[rowSums(is.na(skoda_superb_raw1)) > 0, ]

# Removing all rows with a price less than 10.000kr
skoda_superb_raw1 <- filter(skoda_superb_raw1, price > 10000)

# Setting final vw_passet dataset and removing any rows with NA's
skoda_superb <- skoda_superb_raw1 %>% 
  select(brand, model1, eng_size, eng_type, HP, type, transm, place, kml, km, year, price) %>% 
  na.omit()

names(skoda_superb)[names(skoda_superb) == "model1"] <- "model"


#### Export cleaned up data sets ####
write_tsv(audi_a4, paste('Documents/DataScience/Datasets/bilbasen_audi_a4_', Sys.Date(), '_clean.tsv', sep = ""))
write_tsv(audi_a6, paste('Documents/DataScience/Datasets/bilbasen_audi_a6_', Sys.Date(), '_clean.tsv', sep = ""))
write_tsv(vw_golfVI, paste('Documents/DataScience/Datasets/bilbasen_vw_golfvi_', Sys.Date(), '_clean.tsv', sep = ""))
write_tsv(vw_golfVII, paste('Documents/DataScience/Datasets/bilbasen_vw_golfvii_', Sys.Date(), '_clean.tsv', sep = ""))
write_tsv(vw_passat, paste('Documents/DataScience/Datasets/bilbasen_vw_passat_', Sys.Date(), '_clean.tsv', sep = ""))
write_tsv(skoda_octavia, paste('Documents/DataScience/Datasets/bilbasen_skoda_octavia_', Sys.Date(), '_clean.tsv', sep = ""))
write_tsv(skoda_superb, paste('Documents/DataScience/Datasets/bilbasen_skoda_superb_', Sys.Date(), '_clean.tsv', sep = ""))

