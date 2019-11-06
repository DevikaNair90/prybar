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
La Crosse, WI VA DC 54601")

search_streets(fakeaddresses)
search_streets(fakeaddresses, "df") %>% tidyr::unnest()

search_zipcode(fakeaddresses)

search_zipcode(fakeaddresses, "df") %>% tidyr::unnest()

search_state(fakeaddresses, "df") %>% select(StatesString) %>% tidyr::unnest() #%>% tidyr::unnest()


cities <- read.delim("data/2015_Gaz_place_national.txt", quote = "\t")[c(1,4)]
cities$NAME <- str_replace_all(cities$NAME, "\\\xf1", "n")
cities <- cities %>% 
  filter(USPS != "USPS") %>% 
  mutate(PlaceType = str_extract_all(NAME, pattern = "CDP|town|city|metropol|munic"), # #CDP = str_extract(NAME, pattern = "CDP"),
         NAME2 = str_trim(str_remove_all(string = NAME, pattern = "CDP|town|city")))

states <- unique(cities$USPS)
patt <- vector()
for (i in 1:length(states)) {
  city_state_subset <- cities[cities$USPS == states[i], ]
  patt[i] <- paste0(city_state_subset$NAME2, collapse = "|")
  patt
}

cityregex <- dplyr::tibble(state = states, pattern = patt) %>% filter(state != "USPS")
cityregex
write.csv(cityregex, "data/cityregex.csv", row.names = F)
rm(cityregex)
rm(i)
cityregex <- read.csv("data/cityregex.csv")
View(cityregex)


search_addresses(fakeaddresses) 
search_addresses(fakeaddresses, "df") 
search_addresses(fakeaddresses, "df") %>% tidyr::unnest() 
search_addresses(fakeaddresses, "df") %>% tidyr::unnest(CitiesString) 
search_addresses(fakeaddresses, "df") %>% tidyr::unnest(StreetsString) 
search_cities_in_states(fakeaddresses, "df") %>% tidyr::unnest(CitiesString)
search_streets(fakeaddresses, "df") %>% tidyr::unnest(StreetsString)

ashley_madison[search_DOB(ashley_madison$dob) == TRUE,] %>% nrow()
fakeaddresses[search_addresses(fakeaddresses) == TRUE] 
fakeaddresses[search_cities_in_states(fakeaddresses) == TRUE] 
fakeaddresses[search_streets(fakeaddresses) == TRUE] 

cityregex <- read.csv("privaR/data/cityregex.csv")
countrycodes <- read.csv("privaR/data/country_codes.csv")
stateabbrevs <- read.csv("privaR/data/state_abbrevs.csv")
streetabbrevsusa <- read.csv("privaR/data/street_abbrevs_usa.csv")
streetabbrevs <- read.csv("privaR/data/street_abbrevs.csv")

save(cityregex, countrycodes, stateabbrevs, streetabbrevs, streetabbrevsusa, file = "privaR/R/sysdata.rda")
load("privaR/R/sysdata.rda")
