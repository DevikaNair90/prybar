library(dplyr)
library(stringr)

source("R/search_cities.R")
source("R/search_streets.R")

search_addresses <- function(vec, output) {
  cities <- search_cities_in_states(vec, "df")
  streets <- search_streets(vec, "df") 
  if (any(cities$CitiesYN) & any(streets$StreetsYN)) {
    addresses <- inner_join(cities, streets, by = c("ID", "OriginalString")) %>% 
      mutate(AddressYN = ifelse(CitiesYN == TRUE & StreetsYN == TRUE, TRUE, FALSE)) 
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


