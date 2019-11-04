library(stringr)
library(dplyr)
library(generator)
# install.packages("dplyr")
# install.packages("knitr")

n <- 6
set.seed(1)
ashley_madison <- 
  data.frame(name = r_full_names(n), 
             snn = r_national_identification_numbers(n), 
             dob = r_date_of_births(n), 
             email = r_email_addresses(n), 
             ip = r_ipv4_addresses(n), 
             phone = r_phone_numbers(n), 
             credit_card = r_credit_card_numbers(n), 
             lat = r_latitudes(n), 
             lon = r_longitudes(n), 
             stringsAsFactors = FALSE)
knitr::kable(ashley_madison, format = "markdown")

search_SSN <- function(vec, output) {
  patt <- c("[0-9]{3}( |.|-)?[0-9]{2}( |.|-)?[0-9]{4}")
  #patt <- paste(patt, collapse = "|")
  ssn <- dplyr::tibble(OriginalString = vec,
                         SSNYN = stringr::str_detect(string = vec, pattern = patt), 
                         SSNString = stringr::str_extract_all(string = vec, pattern = patt))
  
  if (missing(output)||output == "vector") {
    return(ssn$SSNYN)
  }
  
  else if (output == "df") {
    return(ssn)
  }
  
  else {
    print("Output argument invalid.")
  }

}

search_phone <- function(vec, output) {
  patt <- c("\\(?[0-9]{3}\\)?( |.|-)?[0-9]{3}( |.|-)?[0-9]{4}", # N America
            #"\\(?[0-9]{3}\\)?( |.|-)?[0-9]{3}( |.|-)?[0-9]{4}", # Mexico - 2 4 4
            #"[0-9]{2} [0-9]{2} [0-9]{2} [0-9]{2} [0-9]{2}", # France - 2 2 2 2 2
            # Belgium 4 3 3 or 4 2 2 2 
            # Denmark 4 4 
            "Tel", "Phone", "Cell", "Mobile", "Fax")
  patt <- paste(patt, collapse = "|")
  phone <- dplyr::tibble(OriginalString = vec,
                        PhoneYN = stringr::str_detect(string = vec, pattern = patt), 
                         PhoneString = stringr::str_extract_all(string = vec, pattern = patt))

  if (missing(output)||output == "vector") {
    return(phone$PhoneYN)
  }

  else if (output == "df") {
    return(phone)
  }
  
  else {
    print("Output argument invalid.")
  } 
  
}

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
  
  dob <- dplyr::tibble(OriginalString = vec,
                       DOBYN = stringr::str_detect(string = vec, pattern = patt), 
                       DOBStsring = stringr::str_extract_all(string = vec, pattern = patt))
  
  if (missing(output)||output == "vector") {
    return(dob$DOBYN)
  }
  
  else if (output == "df") {
    return(dob)
  }
  
  else {
    print("Output argument invalid.")
  }
  
}

ashley_madison %>% mutate(search_DOB(dob))
search_phone(r_phone_numbers(10)) 
search_phone(r_phone_numbers(10), output = "vector") 
search_phone(r_phone_numbers(10), output = "df") 
search_phone(r_phone_numbers(10), output = "dudley") 
search_phone(r_phone_numbers(10, use_hyphens =  T)) 
search_phone(r_phone_numbers(10, use_parentheses = T)) 
search_phone(r_phone_numbers(10, use_spaces =  T)) 

search_SSN(r_national_identification_numbers(10)) 
search_SSN(r_national_identification_numbers(10), output = "vector") 
search_SSN(r_national_identification_numbers(10), output = "df") 
search_SSN(r_national_identification_numbers(10), output = "dudley") 

search_DOB(r_date_of_births(10)) 
search_DOB(r_national_identification_numbers(10), output = "vector") 
search_DOB(r_national_identification_numbers(10), output = "df") 
search_DOB(r_national_identification_numbers(10), output = "dudley") 

ashley_madison %>% mutate(phoneYN = search_phone(phone), 
                          ssnYN = search_SSN(snn))

search_DOB(r_national_identification_numbers(10), output = "df")
search_DOB(r_date_of_births(10), output = "df") 
search_DOB(c("JAN191990", "19JAN1990"))
search_DOB(c("11/1/1990", "12/4/54", "12 04 2019"))
search_DOB(c("January 14, 2010", "June 19, 2019"))
