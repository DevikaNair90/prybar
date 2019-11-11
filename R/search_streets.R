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
  pattdigits <- "[0-9]"
  # pattern to match digits
  streetspatt <- privaR:::streetabbrevsusa 
  # pull street and street names
  streetspatt <- paste(streetspatt$StreetType, streetspatt$Abbrev, streetspatt$Abbrev2, sep = "|")
  streetspatt <- paste0(streetspatt, collapse = "|")
  # pattern to find street names 
  
  threepriorwhitespacespatt <- paste0("(?:\\w+\\W*){3}\\b(", streetspatt, ")", collapse = "|") 
  digitswspacewroadname <- paste0("\\d(\\W*).*(?<=(", streetspatt, "))") #patt_test2
  # pattern to find street addresses
  
  
  streets <- dplyr::tibble(ID = seq.int(length(vec)),
                           OriginalString = vec, #gsub(x = vec, pattern = "\\\n", replacement = ", "),
                           StreetMention =  ifelse(grepl(OriginalString, pattern = pattdigits) == TRUE, 
                                                   grepl(OriginalString, 
                                                         pattern = threepriorwhitespacespatt), FALSE)
                           )
  
  
  
  streets_T <- streets %>% 
    filter(StreetMention == TRUE) %>% 
    mutate(StreetsString = list(character()),
           StreetType = list(character()))
  
  findstreets <- gregexpr(text = streets_T$OriginalString, pattern =  threepriorwhitespacespatt, perl = TRUE)
  StreetsString =  regmatches(x = streets_T$OriginalString, m = findstreets, invert = FALSE)
  
  if (length(StreetsString) > 0 & length(StreetsString) == nrow(streets_T)) {
    for (i in 1:length(StreetsString)) {
      streets_T$StreetsString[[i]] <- StreetsString[[i]]
      streets_T$StreetType[[i]] <- str_extract_all(streets_T$StreetsString[[i]], pattern = streetspatt)
    }
  }
  
  streets_T2 <- streets_T %>% 
    tidyr::unnest() %>% tidyr::unnest()  %>% 
    transmute(ID, patta = paste0("\\d(\\W*).*(?<=(", StreetType, "))"), 
              Address = str_extract(string = StreetsString, pattern = patta)) %>% 
    filter(!is.na(Address) & nchar(Address) > 2) %>% select(ID, Address)
  
  streets_T <- streets_T %>% 
    select(ID, OriginalString, StreetMention) %>% 
    left_join(streets_T2, by = "ID") 
  
  streets_F <- streets %>% 
    filter(StreetMention == FALSE) %>% 
    mutate(Address = list(character()))
  
  streets <- rbind(streets_F, streets_T) %>% arrange(ID) 
  
  
  if (missing(output)||output == "vector") {
    return(streets$StreetMention)
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

