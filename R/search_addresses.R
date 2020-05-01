#' Searches vector for full addresses, including zipcode, city, state, street 
#' name and house number. 
#'
#' This function takes a vector input and searches presence of any text that 
#' appears to be a full address. It first identifies whether any string appears 
#' to be a state name or state abbreviation, then determines whether the 
#' preceding text appears to be a city in that state. Once a city and state are 
#' identified, it then looks for strings that appear to be zipcodes or street 
#' addresses (meaning street name and house number).
#' 
#' The default output is a TRUE/FALSE vector but the function can also
#' return a dataframe of the original vector input, the TRUE/FALSE result, 
#' and the substring that matched the address pattern. 
#'
#' @param vec A vector input whose contents need to be searched for references
#' to addresses
#' @param output The desired output of function. Defaults to "vector" where T/F 
#' vector result is returned. The argument "df" will output a table of original 
#' vector input, T/F vector result, and the matching substring. 
#' @import stringr
#' @import maditr
#' @export
#' @examples
#' 
#' fakeaddresses <- c("820 Nut Swamp Ave.
#' Toms River, NJ 08753",
#'                    "982 Clay Street
#'                    Lenoir, NC 28645",
#'                    "4 NW. Mayfield Rd.
#'                    Springfield Gardens, NY 11413",
#'                    "20 Elmwood Street
#'                    Raleigh, NC 27603",
#'                    "196 E. Green Lake Road
#'                    Birmingham, AL 35209",
#'                    "73 Beechwood Dr.
#'                    La Crosse, WI VA DC 54601")
#' 
#' search_addresses(fakeaddresses)
#' 


# source("R/search_cities.R")
# source("R/search_streets.R")
# source("R/search_zipcode.R")

search_addresses <- function(vec, output) {
  cities <- search_cities_in_states(vec, "dt")
  streets <- search_streets(vec, "dt") 
  zipcodes <- search_zipcode(vec, "dt")
  
  if (any(streets$StreetsYN)) {
    addresses <-streets %>% 
      maditr::dt_full_join(cities, by = c("ID", "OriginalString")) %>% 
      maditr::dt_full_join(zipcodes, by = c("ID", "OriginalString")) %>%
      maditr::dt_mutate(AddressYN = ifelse(StreetsYN == TRUE & (CitiesYN == TRUE|ZipCodeYN == TRUE), TRUE, FALSE)) 
  }
  else {
    addresses <-  maditr::data.table(OriginalString = vec, 
                             AddressYN = FALSE,
                             AddressString = NA)
  }
  
  output <- ifelse(missing(output), "vector", output)
  
  if (output == "vector") {
    return(addresses$AddressYN)
  }
  
  else if (output == "dt") {
    return(addresses)
  }
  
  else {
    print("Output argument invalid.")
  }
  
}


# account for PO box, account for zipcode

