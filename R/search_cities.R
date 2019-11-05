library(stringr)
library(dplyr)
source("R/search_state.R")

search_cities_in_states <- function(vec, output) {
  states <- search_state(vec, "df") 
  
  if (any(states$StatesYN)) {
    statesY <- states %>%
      filter(StatesYN == TRUE) %>% 
      mutate(ID = seq.int(nrow(.))) %>%
      tidyr::unnest(StatesString) %>%
      select(ID, OriginalString, StatesString) %>%
      mutate(OriginalString = str_replace_all(OriginalString, "\n", ", "),
             stringsearchbefore = str_extract(OriginalString,
                                              pattern = paste0("(?:\\w+\\W*){3}\\b", StatesString)))
    
    cityregex <- read.csv("data/cityregex.csv", colClasses = c("character", "character"))
    statesY <- statesY %>% left_join(cityregex, by = c("StatesString" = "state"))
    cities <-  statesY %>% transmute(ID = ID,
                                     OriginalString = OriginalString,
                                     StatesString = StatesString,
                                     CitiesYN = str_detect(string = stringsearchbefore, pattern = pattern),
                                     CitiesString = str_extract_all(string = stringsearchbefore, pattern = pattern)) 
  }
  else {cities <-  dplyr::tibble(OriginalString = vec, 
                           StatesString = NA,
                           CitiesYN = FALSE,
                           CitiesString = NA) 
  }
  
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
