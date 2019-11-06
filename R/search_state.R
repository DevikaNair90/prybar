#' Searches vector for states by full name and two-letter abbreviation.
#'
#' This function takes a vector input and searches presence of 
#' any text that matches states' full names or 2 letter abbreviation. 
#' 
#' The default output is a TRUE/FALSE vector but the function can also
#' return a dataframe of the original vector input, the TRUE/FALSE result, 
#' and the substring that matched the state name pattern. 
#'
#' @param vec A vector input whose contents need to be searched for references
#' to states.
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
#' search_state(fakeaddresses)
#' 


search_state <- function(vec, output) {
  #states <- read.csv("data/state_abbrevs.csv")
  states <- paste(states$State, states$Abbreviation, sep = "|")
  patt <- paste0(states, collapse = "|")
  states <- dplyr::tibble(OriginalString = str_replace_all(vec, "\n", ", "),
                           StatesYN = stringr::str_detect(string = OriginalString, pattern = patt), 
                          StatesString = stringr::str_extract_all(string = OriginalString, pattern = patt))
  
  if (missing(output)||output == "vector") {
    return(states$StatesYN)
  }
  
  else if (output == "df") {
    return(states)
  }
  
  else {
    print("Output argument invalid.")
  }
  
}


