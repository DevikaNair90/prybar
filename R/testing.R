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

search_email(r_email_addresses(10))
search_email(r_email_addresses(10), output = "df")
search_email(r_email_addresses(10), output = "vector")


ashley_madison[search_SSN(ashley_madison$snn) == TRUE, ]

search_zipcode(c("20190", "20854-6056"))
search_zipcode(c("20190", "20854-6056"), "df")


search_state(c("I live in Nebraska", "Mail the package to ME.", "Mail the package to me."), "df") 
search_state(c("I live in Nebraska", "Mail the package to ME.", "Mail the package to me."), "df") %>% tidyr::unnest() 

streets <- read.csv("Documents/GitHub/privaR/data/street_abbrevs.csv", na.strings = "", 
                    stringsAsFactors = F, 
                    col.names = c("street_type_suffix", "canada_abbrev", "usa_ups_abbrev", "edmonton_abbrev")) %>%
  select(street_type_suffix, usa_ups_abbrev) %>% filter(!is.na(usa_ups_abbrev))

streets2 <- tidyr::unnest(tibble("StreetType" = str_split(streets$street_type_suffix, "\\sor\\s"), "Abbrev" = str_split(streets$usa_ups_abbrev, "\\sor\\s")))

streets2 <- streets2 %>% mutate(Abbrev2 = paste0(str_extract(Abbrev, "^[:alpha:]"), str_to_lower(str_extract(Abbrev, "(?<=.{1}).+"))))

write.csv(streets2, "Documents/GitHub/privaR/data/street_abbrevs_usa.csv", row.names = F)


fakeaddresses <- c("820 Nut Swamp Ave.
Toms River, NJ 08753",
"982 Clay Street
Lenoir, NC 28645",
"4 NW. Mayfield Rd.
Springfield Gardens, NY 11413",
"20 Elmwood Street
Raleigh, NC 27603",
"196 E. Green Lake Road
Birmingham, AL 35209",
"73 Beechwood Dr.
La Crosse, WI 54601")

search_streets(fakeaddresses)
search_streets(fakeaddresses, "df") %>% tidyr::unnest()

search_zipcode(fakeaddresses)

search_zipcode(fakeaddresses, "df") %>% tidyr::unnest()


