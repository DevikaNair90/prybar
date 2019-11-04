library(stringr)
library(dplyr)


search_email <- function(vec, output) {
  patt <- "([^\\s]+)@([^\\s]+)"
  email <- dplyr::tibble(OriginalString = vec,
                       EmailYN = stringr::str_detect(string = vec, pattern = patt), 
                       EmailString = stringr::str_extract_all(string = vec, pattern = patt))
  
  if (missing(output)||output == "vector") {
    return(email$EmailYN)
  }
  
  else if (output == "df") {
    return(email)
  }
  
  else {
    print("Output argument invalid.")
  }
  
}


