#' Searches vector for zipcodes.
#'
#' This function takes a vector input and searches presence of any text that 
#' appears to be a zipcode by looking for sequences of numbers. Specifically,
#' it looks for 5 digit sequences, 9 digit sequences, and 5 digit sequences 
#' followed by 4 digit sequences with some kind of separator.
#' The default output is a TRUE/FALSE vector but the function can also
#' return a dataframe of the original vector input, the TRUE/FALSE result, 
#' and the substring that matched the zipcode pattern. 
#'
#' @param vec A vector input whose contents need to be searched for references
#' to zipcodes
#' @param output The desired output of function. Defaults to "vector" where T/F 
#' vector result is returned. The argument "dt" will output a table of original 
#' vector input, T/F vector result, and the matching substring. 
#' @import stringr
#' @import data.table
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
#' search_zipcode(fakeaddresses)
#' 

search_zipcode <- function(vec, output) {
  patt <- "\\d{5}(( |.|-)?\\d{4})?"
  zipcode <- data.table::data.table(ID = seq.int(length(vec)),
                        OriginalString = gsub(x = vec, pattern =  "\n", replacement = ", ")) 
  
  ms <- regmatches(zipcode$OriginalString, 
                   gregexpr(pattern = patt, zipcode$OriginalString))  
  zipcode$ZipCodeString <- ms
  zipcode$ZipCodeYN <- ifelse(lengths(zipcode$ZipCodeString) > 0, TRUE, FALSE)
  
  output <- ifelse(missing(output), "vector", output)
  
  if (output == "vector") {
    return(zipcode$ZipCodeYN)
  }
  
  else if (output == "dt") {
    return(zipcode)
  }
  
  else {
    print("Output argument invalid.")
  }
  
}


