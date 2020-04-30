#' Searches vector for phone numbers.
#'
#' This function takes a vector input and searches presence of any text that 
#' appears to be a US phone number. Specifically, this function searches for 
#' sequences of 10 digits with separators at the 3-3-4 mark and/or parentheses 
#' around the first 3 digits. 
#' 
#' The default output is a TRUE/FALSE vector but the function can also
#' return a dataframe of the original vector input, the TRUE/FALSE result, 
#' and the substring that matched a phone pattern. 
#'
#' @param vec A vector input whose contents need to be searched for references
#' to phone numbers
#' @param output The desired output of function. Defaults to "vector" where T/F 
#' vector result is returned. The argument "df" will output a table of original 
#' vector input, T/F vector result, and the matching substring. 
#' @import stringr
#' @import maditr
#' @import data.table
#' @suggest generator
#' @export
#' @examples
#' set.seed(2)
#' testcase <- data.frame(name = generator::r_full_names(2),  
#' snn = generator::r_national_identification_numbers(2), 
#' dob = generator::r_date_of_births(2), 
#' email = generator::r_email_addresses(2), 
#' ip = generator::r_ipv4_addresses(2), 
#' phone = generator::r_phone_numbers(2),  
#' credit_card = generator::r_credit_card_numbers(2), 
#' lat = generator::r_latitudes(2), 
#' lon = generator::r_longitudes(2), 
#' stringsAsFactors = FALSE)
#' 
#' search_phone(testcase$phone)
#' 

search_phone <- function(vec, output) {
  patt <- c("(?<![0-9])\\(?[0-9]{3}\\)?( |\\.|-)?[0-9]{3}( |\\.|-)?[0-9]{4}(?![0-9])", # N America
            #"\\(?[0-9]{3}\\)?( |.|-)?[0-9]{3}( |.|-)?[0-9]{4}", # Mexico - 2 4 4
            #"[0-9]{2} [0-9]{2} [0-9]{2} [0-9]{2} [0-9]{2}", # France - 2 2 2 2 2
            # Belgium 4 3 3 or 4 2 2 2 
            # Denmark 4 4 
            "Tel", "Phone", "Cell", "Mobile", "Fax")
  patt <- paste(patt, collapse = "|")
  
  # phone <- as.data.frame(cbind(OriginalString = vec, 
  #                              PhoneString =  regmatches(vec , m = gregexpr(vec, pattern = patt )),
  #                              PhoneYN = ))
  
  phone <- data.table::data.table(OriginalString = vec,
                         PhoneYN = stringr::str_detect(string = vec, pattern = patt), 
                         PhoneString = stringr::str_extract_all(string = vec, pattern = patt))
  
  areacodes <-read.csv("https://raw.githubusercontent.com/ravisorg/Area-Code-Geolocation-Database/master/us-area-code-cities.csv", header=F)
  
  output <- ifelse(missing(output), "vector", output)
  
  if (output == "vector") {
    return(phone$PhoneYN)
  }
  
  else if (output == "df") {
    return(phone)
  }
  
  else {
    print("Output argument invalid.")
  } 
  
}
