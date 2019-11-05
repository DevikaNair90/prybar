library(stringr)
library(dplyr)


search_zipcode <- function(vec, output) {
  patt <- "\\d{5}(( |.|-)?\\d{4})?"
  zipcode <- dplyr::tibble(ID = seq.int(length(vec)),
                           OriginalString = str_replace_all(vec, "\n", ", "),
                           ZipCodeYN = stringr::str_detect(string = OriginalString, pattern = patt), 
                           ZipCodeString = stringr::str_extract_all(string = OriginalString, pattern = patt))
  
  if (missing(output)||output == "vector") {
    return(zipcode$ZipCodeYN)
  }
  
  else if (output == "df") {
    return(zipcode)
  }
  
  else {
    print("Output argument invalid.")
  }
  
}


