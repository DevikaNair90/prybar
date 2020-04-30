#' Searches vector for emails.
#'
#' This function takes a vector input and searches presence of any text that 
#' appears to be an email.
#' 
#' The default output is a TRUE/FALSE vector but the function can also
#' return a dataframe of the original vector input, the TRUE/FALSE result, 
#' and the substring that matched the email text pattern. 
#'
#' @param vec A vector input whose contents need to be searched for references
#' to emails
#' @param output The desired output of function. Defaults to "vector" where T/F 
#' vector result is returned. The argument "dt" will output a table of original 
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
#' search_email(testcase$email)

search_email <- function(vec, output) {
  patt <- "([^\\s]+)@([^\\s]+)"
  email <- data.table::data.table(OriginalString = vec,
                       EmailYN = stringr::str_detect(string = vec, pattern = patt), 
                       EmailString = stringr::str_extract_all(string = vec, pattern = patt))
  
  if (missing(output)||output == "vector") {
    return(email$EmailYN)
  }
  
  else if (output == "dt") {
    return(email)
  }
  
  else {
    print("Output argument invalid.")
  }
  
}


