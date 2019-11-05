library(stringr)
library(dplyr)

search_state <- function(vec, output) {
  states <- read.csv("data/state_abbrevs.csv")
  states <- paste(states$State, states$Abbreviation, sep = "|")
  patt <- paste0(states, collapse = "|")
  states <- dplyr::tibble(OriginalString = str_replace_all(vec, "\n", ", "),
                           StatesYN = stringr::str_detect(string = OriginalString, pattern = patt), 
                          StatesString = stringr::str_extract_all(string = OriginalString, pattern = patt))
  
  if (missing(output)||output == "vector") {
    return(states$StatesYN)
  }
  
  else if (output == "df") {
    return(states)
  }
  
  else {
    print("Output argument invalid.")
  }
  
}


