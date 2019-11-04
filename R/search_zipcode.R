library(stringr)
library(dplyr)


search_zipcode <- function(vec, output) {
  patt <- "\\d{5}(( |.|-)?\\d{4})?"
  zipcode <- dplyr::tibble(OriginalString = vec,
                         ZipCodeYN = stringr::str_detect(string = vec, pattern = patt), 
                         ZipCodeString = stringr::str_extract_all(string = vec, pattern = patt))
  
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


