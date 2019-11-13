
setwd("~/Documents/git_dn90/")
devtools::install("privaR")
library(privaR)

devtools::document("privaR")
setwd("~/Documents/git_dn90/privaR/")
library(devtools)
use_data(cityregex, countrycodes, stateabbrevs, streetabbrevs, streetabbrevsusa, 
         internal = TRUE, overwrite = TRUE)


?search_state
?search_addresses()
?search_zipcode()

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

search_addresses(fakeaddresses)

search_email(fakeaddresses)
search_email(fakeaddresses, "df")
search_state(fakeaddresses)
search_cities_in_states(fakeaddresses)

search_streets(fakeaddresses)
search_streets(fakeaddresses, "df")
search_streets(head(va_personal$cityname, 100), "df")
a[a$StreetMention == TRUE,] %>% tidyr::unnest() %>% View()


b <- privaR::pii_table(va_personal, "~/Desktop/testfolder/")

ssn_results <- readRDS("~/Desktop/testfolder/va_personal_PII_2_SSN_2019-11-13.RDS")

ssn_result_msa <- ssn_results[4]
c_ssn <- pii_table(va_personal, "~/Desktop/testfolder/")
ssn_results2 <- readRDS("~/Desktop/testfolder/va_personal_PII_2_SSN_2019-11-13.RDS")
ssn_results2

d <- privaR::pii_table(va_personal, "~/Desktop/testfolder/")

e <- privaR::pii_geo_table(va_personal, "~/Desktop/testfolder/")
