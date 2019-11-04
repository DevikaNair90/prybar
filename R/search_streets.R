library(stringr)
library(dplyr)

search_streets <- function(vec, output) {
  streets <- read.csv("data/street_abbrevs_usa.csv")
  streets <- paste(streets$StreetType, streets$Abbrev, streets$Abbrev2, sep = "|")
  patt <- paste0("\\d+.*", "(", streets, ")", collapse = "|")
  streets <- dplyr::tibble(ID = seq.int(length(vec)),
                           OriginalString = str_replace_all(vec, "\n", ", "),
                          StreetsYN = stringr::str_detect(string = OriginalString, pattern = patt), 
                          StreetsString = stringr::str_extract_all(string = OriginalString, pattern = patt))
  
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

