
setwd("~/Documents/GitHub/")
devtools::install("prybar")
library(prybar)

devtools::document("prybar")
setwd("~/Documents/git_dn90/prybar")
library(devtools)
use_data(cityregex, countrycodes, stateabbrevs, streetabbrevs, streetabbrevsusa, 
         internal = TRUE, overwrite = TRUE)

install.packages("changer")

content <- letters
package.skeleton("privaR", path = tempdir())
readLines(file.path(tempdir(), "privaR", "DESCRIPTION"))
library(changer)
changer(file.path(tempdir(), "privaR"), 
        new_name = "prybar", check_validity = FALSE, ask = FALSE)
readLines(file.path(tempdir(), "prybar", "DESCRIPTION"))
unlink(file.path(tempdir(), "prybar"), recursive = TRUE)


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



privaR::pii_geo_table(fakeaddresses, "~/Desktop/testfolder/New Folder With Items/")

fakeaddressessummary <- readRDS("~/Desktop/testfolder/New Folder With Items/fakeaddresses_PII_0_Summary_2019-11-13.RDS")
fakeaddressesstates <- readRDS("~/Desktop/testfolder/New Folder With Items/fakeaddresses_PII_1_STATES_2019-11-13.RDS")
fakeaddressescities <- readRDS("~/Desktop/testfolder/New Folder With Items/fakeaddresses_PII_2_CITIES_2019-11-13.RDS")
fakeaddresseszipcodes <- readRDS("~/Desktop/testfolder/New Folder With Items/fakeaddresses_PII_3_ZIPCODES_2019-11-13.RDS")
fakeaddressesstreets <- readRDS("~/Desktop/testfolder/New Folder With Items/fakeaddresses_PII_4_STREETS_2019-11-13.RDS")

vec <- va_personal[1:20,]$bgtresid

search_streets(vec, "df")

e <- privaR::search_addresses(va_personal$cityname, "df")
e <- privaR::pii_geo_table(fakeaddresses, path = "~/Desktop/testfolder/Nov13_2/") 
e <- privaR::pii_geo_table(va_personal, path = "~/Desktop/testfolder/Nov13_2/") 
f <- privaR::pii_geo_table(va_personal, writeout = FALSE) 
  #privaR::pii_geo_table(va_personal, path = "~/Desktop/testfolder/Nov13_2/") 
f <- privaR::search_streets(va_personal$bgtresid, "df")
g <- privaR::search_streets(va_personal$statename)
h <- privaR::search_streets(va_personal$cityname, "df")


more_tests <- pii_geo_table(fakeaddresses, writeout = FALSE)

losangeles <- read.csv("prybar/los-angeles-listing-of-businesses/listing-of-all-businesses.csv")

nrow(losangeles)
colnames(losangeles)
la_test <- losangeles %>% dplyr::transmute(paste(`LOCATION.ACCOUNT..`, 
                                          BUSINESS.NAME, 
                                          DBA.NAME, 
                                          STREET.ADDRESS, 
                                          CITY, "CA", ZIP.CODE , 
                                          LOCATION.DESCRIPTION, 
                                          MAILING.ADDRESS, MAILING.CITY, 
                                          MAILING.ZIP.CODE, NAICS, PRIMARY.NAICS.DESCRIPTION,  COUNCIL.DISTRICT,  
                                          LOCATION.START.DATE, LOCATION.END.DATE,  LOCATION, Neighborhood.Councils..Certified.,
                                          LA.Specific.Plans, Precinct.Boundaries, Council.Districts, Census.Tracts,        
                                          Zip.Codes, sep = " "))

colnames(la_test) <- c("test")

View(head(la_test))

Sys.time()
la_test_statetiming <- search_state(la_test, "dt")
Sys.time()