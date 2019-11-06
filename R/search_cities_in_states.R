#' Searches vector for cities (based on whether vector includes state). 
#'
#' This function takes a vector input and searches presence of #' any text that 
#' appears to be a zipcode by looking for sequences of numbers. Specifically,
#' it first identifies whether any string appears to be a state name or state 
#' abbreviation, then determines whether the preceding text appears to be a city 
#' in that state.
#' 
#' The default output is a TRUE/FALSE vector but the function can also
#' return a dataframe of the original vector input, the TRUE/FALSE result, 
#' and the substring that matched the city/state pattern. 
#'
#' @param vec A vector input whose contents need to be searched for references
#' to city names
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
#' search_cities_in_states(fakeaddresses)
#' 


search_cities_in_states <- function(vec, output) {
  states <- search_state(vec, "df") 
  
  if (any(states$StatesYN)) {
    statesY <- states %>%
      filter(StatesYN == TRUE) %>% 
      mutate(ID = seq.int(nrow(.))) %>%
      tidyr::unnest(StatesString) %>%
      select(ID, OriginalString, StatesString) %>%
      mutate(OriginalString = str_replace_all(OriginalString, "\n", ", "),
             stringsearchbefore = str_extract(OriginalString,
                                              pattern = paste0("(?:\\w+\\W*){3}\\b", StatesString)))
    
    cityregex <- read.csv("data/cityregex.csv", colClasses = c("character", "character"))
    statesY <- statesY %>% left_join(cityregex, by = c("StatesString" = "state"))
    cities <-  statesY %>% transmute(ID = ID,
                                     OriginalString = OriginalString,
                                     StatesString = StatesString,
                                     CitiesYN = str_detect(string = stringsearchbefore, pattern = pattern),
                                     CitiesString = str_extract_all(string = stringsearchbefore, pattern = pattern)) 
  }
  else {cities <-  dplyr::tibble(OriginalString = vec, 
                           StatesString = NA,
                           CitiesYN = FALSE,
                           CitiesString = NA) 
  }
  
  if (missing(output)||output == "vector") {
    return(cities$CitiesYN)
  }
  
  else if (output == "df") {
    return(cities)
  }
  
  else {
    print("Output argument invalid.")
  }
}