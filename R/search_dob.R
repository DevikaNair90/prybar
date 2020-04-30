#' Searches vector for dates.
#'
#' This function takes a vector input and searches presence of any text that 
#' appears to be a date.
#' 
#' The default output is a TRUE/FALSE vector but the function can also
#' return a dataframe of the original vector input, the TRUE/FALSE result, 
#' and the substring that matched a date pattern. 
#'
#' @param vec A vector input whose contents need to be searched for references
#' to dates (eg. date of birth)
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
#' search_dob(testcase$dob)
#' 
#' 
# Doesn't start with a digit (?<![0-9])
# 1st - month maybe a 0 or 1 then has to have a 0-9   ([0-1])?[0-9]
# 2nd - has to have a separator (space, period, dash, forward slash) ( |\\.|-|\\/)
# 3rd - date maybe a 0, 1, 2, 3 then has to have a 0-9 ([0-3])?[0-9]
# 4th - has to have a separator (space, period, dash, forward slash) ( |\\.|-|\\/)
# 5th - year maybe a 19 or 20, but has to have TWO 0-9 digits (19|20)?[0-9][0-9]
# Doesn't end with a digit  (?![0-9])


# note this function really checks for dates, is there a way to add piece where it checks if column name or text of cell includes terms like birth, dob, etc...? 

search_DOB <- function(vec, output) {
  months = c("[J|j][A|a][N|n]([U|u][A|a][R|r][Y|y])?", "[F|f][E|e][B|b]([R|r][U|u][A|a][R|r][Y|y])?",
             "[M|m][A|a][R|r]([C|c][H|h])?", "[A|a][P|p]([R|r][I|i][L|l])?",
             "[M|m][A|a][Y|y]", "[J|j][U|u][N|n]([E|e])?", "[J|j][U|u][L|l]([Y|y])?",
             "[A|a][U|u][G|g]([U|u][S|s][T|t])?", "[S|s][E|e][P|p]([T|t][E|e][M|m][B|b][E|e][R|r])?",
             "[O|o][C|c][T|t]([O|o][B|b][E|e][R|r])?", "[N|n][O|o][V|v]([E|e][M|m][B|b][E|e][R|r])?",
             "[D|d][E|e][C|c]([E|e][M|m][B|b][E|e][R|r])?")
  months <- paste0(months,collapse = "|")
  nostartingdig <- "(?<![0-9])"
  monthpatt <- "([0-1])?[0-9]"
  seppatt <- "( |\\.|-|\\/)"
  datepatt <- "([0-3])?[0-9]"
  yearpatt <- "(19|20)?[0-9][0-9]"
  noendingdig <- "(?![0-9])"
  
  mdypatt <- paste0(nostartingdig, monthpatt, seppatt, datepatt, seppatt, yearpatt, noendingdig)
  dmypatt <- paste0(nostartingdig, datepatt, seppatt, monthpatt, seppatt, yearpatt, noendingdig)
  ymdpatt <- paste0(nostartingdig, yearpatt, seppatt, monthpatt, seppatt, datepatt, noendingdig)
  ydmpatt <- paste0(nostartingdig, yearpatt, seppatt, datepatt, seppatt, monthpatt, noendingdig)
  
  dmoypatt <- paste0(nostartingdig, datepatt, seppatt, "?", "(", months, ")", seppatt, "?", yearpatt, noendingdig)
  modypatt <- paste0(nostartingdig, "(", months, ")", seppatt, "?", datepatt, seppatt, "?", yearpatt, noendingdig)
  ymodpatt <- paste0(nostartingdig, yearpatt, seppatt, "?", "(", months, ")", seppatt, "?", datepatt, noendingdig)
  ydmopatt <- paste0(nostartingdig, yearpatt, seppatt, "?", "(", months, ")", seppatt, "?", monthpatt, noendingdig)
  
  writtenpatt <- paste0(nostartingdig, "(", months, ")", "\\W", datepatt, ",\\W", yearpatt, noendingdig)
  
  patt <- paste(mdypatt, dmypatt, ymdpatt, ydmpatt, 
                modypatt, dmoypatt, ymodpatt, ydmopatt,  
                writtenpatt,
                sep = "|")
  
  dob <- data.table::data.table(OriginalString = vec,
                       DOBYN = stringr::str_detect(string = vec, pattern = patt), 
                       DOBStsring = stringr::str_extract_all(string = vec, pattern = patt))
  
  output <- ifelse(missing(output), "vector", output)
  
  if (output == "vector") {
    return(dob$DOBYN)
  }
  
  else if (output == "df") {
    return(dob)
  }
  
  else {
    print("Output argument invalid.")
  }
  
}

