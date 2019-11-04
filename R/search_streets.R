library(stringr)
library(dplyr)

search_streets <- function(vec, output) {
  streets <- read.csv("Documents/GitHub/privaR/data/street_abbrevs_usa.csv")
  streets <- paste(streets$StreetType, streets$Abbrev, streets$Abbrev2, sep = "|")
  patt <- paste0("\\d+.*", "(", streets, ")", collapse = "|")
  streets <- dplyr::tibble(OriginalString = vec,
                          StreetsYN = stringr::str_detect(string = vec, pattern = patt), 
                          StreetsString = stringr::str_extract_all(string = vec, pattern = patt))
  
  if (missing(output)||output == "vector") {
    return(streets$StreetsYN)
  }
  
  else if (output == "df") {
    return(streets)
  }
  
  else {
    print("Output argument invalid.")
  }
  
}

# can have multiple street names in street name
# can have apartment units

