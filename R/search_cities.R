library(stringr)
library(dplyr)
source("Documents/GitHub/privaR/R/search_state.R")

search_cities_in_states <- function(vec, output) {
  states <- search_state(vec, "df") #%>% select(StatesString) #%>% tidyr::unnest(cols = c(StatesString))
  states <- 
  cities <- read.delim("Documents/GitHub/privaR/data/2015_Gaz_place_national.txt", quote = "\t")[c(1,4)]
  cities <- 
  patt <- paste0(states, collapse = "|")
  cities <- dplyr::tibble(OriginalString = vec,
                          CitiesYN = stringr::str_detect(string = vec, pattern = patt), 
                          CitiesString = stringr::str_extract_all(string = vec, pattern = patt))
  
  if (missing(output)||output == "vector") {
    return(cities$CitiesYN)
  }
  
  else if (output == "df") {
    return(cities)
  }
  
  else {
    print("Output argument invalid.")
  }
  
}
