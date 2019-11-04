library(generator)


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


