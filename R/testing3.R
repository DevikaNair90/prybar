library(RPostgreSQL)
library(maditr)
library(dplyr)
library(stringr)
Sys.setenv(db_userid = "dtn2ep")
#remotes::install_github("dads2busy/dataplumbr")

conn <-dbConnect(drv=PostgreSQL(),
                 dbname="sdad_data",
                 host="sdad.policy-analytics.net",
                 port = 5436, #postgis_1 5436 - postgis_2
                 user=Sys.getenv("db_userid"),
                 password=Sys.getenv("db_userid"))

va_personal = dbReadTable(conn = conn,name = c("bgt", "va_res_pers"))

#va_personal_pii2 <- pii_table(va_personal)
#va_personal_pii2

source("R/search_addresses.R")
source("R/search_dob.R")
source("R/search_email.R")
source("R/search_phone.R")
source("R/search_ssn.R")

colnames(va_personal)
columnsummary = data.frame("Column" = colnames(va_personal))

va_personal_emails <- list()
va_personal_ssn <- list()
va_personal_dob <- list()
va_personal_phone <- list()
va_personal_zipcode <- list()

for (i in 1:ncol(va_personal)) {
  # va_personal_emails[[i]] <- search_email(va_personal[,i], "df")
  # columnsummary$EmailYN[i] <- any(va_personal_emails[[i]]$EmailYN)
  # columnsummary$EmailCount[i] <- sum(va_personal_emails[[i]]$EmailYN, na.rm = TRUE)
  # 
  # va_personal_ssn[[i]] <- search_SSN(va_personal[,i], "df")
  # columnsummary$SSNYN[i] <- any(va_personal_ssn[[i]]$SSNYN)
  # columnsummary$SSNCount[i] <- sum(va_personal_ssn[[i]]$SSNYN, na.rm = TRUE)
  # 
  # va_personal_dob[[i]] <- search_DOB(va_personal[,i], "df")
  # columnsummary$DOBYN[i] <- any(va_personal_dob[[i]]$DOBYN)
  # columnsummary$DOBCount[i] <- sum(va_personal_dob[[i]]$DOBYN, na.rm = TRUE)
  # 
  # va_personal_phone[[i]] <- search_phone(va_personal[,i], "df")
  # columnsummary$PhoneYN[i] <- any(va_personal_phone[[i]]$PhoneYN)
  # columnsummary$PhoneCount[i] <- sum(va_personal_phone[[i]]$PhoneYN, na.rm = TRUE)
  # 
  # va_personal_zipcode[[i]] <- search_zipcode(va_personal[,i], "df")
  # columnsummary$ZipCodeYN[i] <- any(va_personal_zipcode[[i]]$ZipCodeYN)
  # columnsummary$ZipCodeCount[i] <- sum(va_personal_zipcode[[i]]$ZipCodeYN, na.rm = TRUE)
  
  va_personal_address[[i]] <- search_addresses(va_personal[,i], "df")
  columnsummary$AddressYN[i] <- any(va_personal_address[[i]]$AddressYN)
  columnsummary$AddressCount[i] <- sum(va_personal_address[[i]]$AddressYN, na.rm = TRUE)
}

columnsummary
rm(i)

write.csv(columnsummary, "~/Documents/UVA R/BG Resumes/testingpii_2.csv", row.names = F)
# saveRDS(va_personal_emails, "~/Documents/UVA R/BG Resumes/va_personal_emails.RDS")
# saveRDS(va_personal_ssn, "~/Documents/UVA R/BG Resumes/va_personal_ssn.RDS")
# saveRDS(va_personal_dob, "~/Documents/UVA R/BG Resumes/va_personal_dob.RDS")
# saveRDS(va_personal_phone, "~/Documents/UVA R/BG Resumes/va_personal_phone.RDS")
# saveRDS(va_personal_zipcode, "~/Documents/UVA R/BG Resumes/va_personal_zipcode.RDS")
saveRDS(va_personal_address, "~/Documents/UVA R/BG Resumes/va_personal_address.RDS")