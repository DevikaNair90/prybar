library(stringr)
library(dplyr)

search_phone <- function(vec, output) {
  patt <- c("(?<![0-9])\\(?[0-9]{3}\\)?( |\\.|-)?[0-9]{3}( |\\.|-)?[0-9]{4}(?![0-9])", # N America
            #"\\(?[0-9]{3}\\)?( |.|-)?[0-9]{3}( |.|-)?[0-9]{4}", # Mexico - 2 4 4
            #"[0-9]{2} [0-9]{2} [0-9]{2} [0-9]{2} [0-9]{2}", # France - 2 2 2 2 2
            # Belgium 4 3 3 or 4 2 2 2 
            # Denmark 4 4 
            "Tel", "Phone", "Cell", "Mobile", "Fax")
  patt <- paste(patt, collapse = "|")
  phone <- dplyr::tibble(OriginalString = vec,
                         PhoneYN = stringr::str_detect(string = vec, pattern = patt), 
                         PhoneString = stringr::str_extract_all(string = vec, pattern = patt))
  
  if (missing(output)||output == "vector") {
    return(phone$PhoneYN)
  }
  
  else if (output == "df") {
    return(phone)
  }
  
  else {
    print("Output argument invalid.")
  } 
  
}
