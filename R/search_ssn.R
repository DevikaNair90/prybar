library(stringr)
library(dplyr)


search_SSN <- function(vec, output) {
  patt <- c("(?<![0-9])[0-9]{3}( |\\.|-)?[0-9]{2}( |\\.|-)?[0-9]{4}(?![0-9])")
  #patt <- paste(patt, collapse = "|")
  ssn <- dplyr::tibble(OriginalString = vec,
                         SSNYN = stringr::str_detect(string = vec, pattern = patt), 
                         SSNString = stringr::str_extract_all(string = vec, pattern = patt))
  
  if (missing(output)||output == "vector") {
    return(ssn$SSNYN)
  }
  
  else if (output == "df") {
    return(ssn)
  }
  
  else {
    print("Output argument invalid.")
  }

}


