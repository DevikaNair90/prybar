library(dplyr)
library(stringr)

source("R/search_cities.R")
source("R/search_streets.R")
source("R/search_zipcode.R")

search_addresses <- function(vec, output) {
  cities <- search_cities_in_states(vec, "df")
  streets <- search_streets(vec, "df") 
  zipcodes <- search_zipcode(vec, "df")
  
  if (any(streets$StreetsYN)) {
    addresses <-streets %>% 
      full_join(cities, by = c("ID", "OriginalString")) %>% 
      full_join(zipcodes, by = c("ID", "OriginalString")) %>%
      mutate(AddressYN = ifelse(StreetsYN == TRUE & (CitiesYN == TRUE|ZipCodeYN == TRUE), TRUE, FALSE)) 
  }
  else {
    addresses <-  dplyr::tibble(OriginalString = vec, 
                             AddressYN = FALSE,
                             AddressString = NA)
  }
  
  if (missing(output)||output == "vector") {
    return(addresses$AddressYN)
  }
  
  else if (output == "df") {
    return(addresses)
  }
  
  else {
    print("Output argument invalid.")
  }
  
}


# account for PO box, account for zipcode

