#' Searches vector for street address locations, specifically a street name and 
#' a house number. 
#'
#' This function takes a vector input and searches presence of any text that 
#' appears to be a street address. Specifically, it looks for strings that begin
#' with any number of numbers and end with common street names or abbreviations 
#' as sourced by US Post Office  (ex. street, lane, road, st, ln, rd).
#' The default output is a TRUE/FALSE vector but the function can also
#' return a dataframe of the original vector input, the TRUE/FALSE result, 
#' and the substring that matched the street pattern. 
#'
#' @param vec A vector input whose contents need to be searched for references
#' to street addresses
#' @param output The desired output of function. Defaults to "vector" where T/F 
#' vector result is returned. The argument "df" will output a table of original 
#' vector input, T/F vector result, and the matching substring. 
#' @import stringr
#' @import dplyr
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
#' search_streets(fakeaddresses)
#' 

search_streets <- function(vec, output) {
  streets <- privaR:::streetabbrevsusa
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

