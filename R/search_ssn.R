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

search_DOB <- function(vec, output) {
  # months = c("[J|j][A|a][N|n]([U|u][A|a][R|r][Y|y])?", "[F|f][E|e][B|b]([R|r][U|u][A|a][R|r][Y|y])?", 
  #           "[M|m][A|a][R|r]([C|c][H|h])?", "[A|a][P|p]([R|r][I|i][L|l])?", 
  #            "[M|m][A|a][Y|y]", "[J|j][U|u][N|n]([E|e])?", "[J|j][U|u][L|l]([Y|y])?", 
  #            "[A|a][U|u][G|g]([U|u][S|s][T|t])?", "[S|s][E|e][P|p]([T|t][E|e][M|m][B|b][E|e][R|r])?",
  #            "[O|o][C|c][T|t]([O|o][B|b][E|e][R|r])?", "[N|n][O|o][V|v]([E|e][M|m][B|b][E|e][R|r])?",
  #            "[D|d][E|e][C|c]([E|e][M|m][B|b][E|e][R|r])?")
  patt = c("(?<![0-9])([0-1])?[0-9]( |\\.|-|\\/)([0-3])?[0-9]( |\\.|-|\\/)(19|20)?[0-9][0-9](?![0-9])", 
           # Doesn't start with a digit (?<![0-9])
           # 1st - month maybe a 0 or 1 then has to have a 0-9   ([0-1])?[0-9]
           # 2nd - has to have a separator (space, period, dash, forward slash) ( |\\.|-|\\/)
           # 3rd - date maybe a 0, 1, 2, 3 then has to have a 0-9 ([0-3])?[0-9]
           # 4th - has to have a separator (space, period, dash, forward slash) ( |\\.|-|\\/)
           # 5th - year maybe a 19 or 20, but has to have TWO 0-9 digits (19|20)?[0-9][0-9]
           # Doesn't end with a digit  (?![0-9])
           "(?<![0-9])([0-3])?[0-9]( |\\.|-|\\/)([0-1])?[0-9]( |\\.|-|\\/)(19|20)?[0-9][0-9](?![0-9])", 
           # no starting digit, DATE, SEP, MONTH, SEP, YEAR, no ending digit 
           "(?<![0-9])(19|20)?[0-9][0-9]( |\\.|-|\\/)([0-1])?[0-9]( |\\.|-|\\/)([0-3])?[0-9](?![0-9])"
           # no starting digit, YEAR, SEP, MONTH, SEP, DATE, no ending digit
           )
  #patt = paste0(patt, collapse = "|")
  # patt <- paste(months, collapse = "|")
  dob <- dplyr::tibble(OriginalString = vec,
                       DOBYN = stringr::str_detect(string = vec, pattern = patt), 
                       DOBString = stringr::str_extract_all(string = vec, pattern = patt))
  
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

search_DOB(r_national_identification_numbers(10), output = "df")  %>%  tidyr::unnest()
stringr::str_extract_all(string = c("question", "qweef"), pattern =  "q(?!u)")
stringr::str_extract_all(string = c("12/19/2019", "012-19-2019", "12-19-20190"), 
                         pattern =  "(?<![0-9])([0-1])?[0-9]( |\\.|-|\\/)([0-2])?[0-9]( |\\.|-|\\/)(19|20)?[0-9][0-9](?![0-9])")
