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
#' @import maditr
#' @import data.table
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
  # DATASETS
  streetspatt <- privaR:::streetabbrevsusa 
  
  # PATTERNS - numbers, street abbrevaiations and names
  # pattern to match digits
  pattdigits <- "\\d" 
  # pattern to find street names 
  streetspatt <- paste(streetspatt$StreetType, streetspatt$Abbrev, streetspatt$Abbrev2, sep = "|") # street abbreviations
  streetspatt <- paste0(streetspatt, collapse = "|") # street abbreviations 
  threepriorwhitespacespatt <- paste0("(?:\\w+\\W*){3}\\b(", streetspatt, ")", collapse = "|") 
  # pattern to find street addresses
  
  streets <- data.table::data.table(ID = seq.int(length(vec)),
                                    OriginalString = gsub(x = vec, pattern =  "\n", replacement = ", ")) 
  
  ms <- regmatches(streets$OriginalString,
                   gregexpr(pattern = threepriorwhitespacespatt, text = streets$OriginalString))
  
  streets$StreetString <- ms
  streets$StreetYN <- ifelse(lengths(streets$StreetString) > 0, TRUE, FALSE)
  
  output <- ifelse(missing(output), "vector", output)
  
  if (output == "vector") {
    return(streets$StreetYN)
  }
  
  
  else if (output == "dt") {
    return(streets)
  }
  
  
  else {
    print("Output argument invalid.")
  }
  
}

# can have multiple street names in street name
# can have apartment units

