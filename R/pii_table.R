library(dplyr)
library(stringr)

source("R/search_addresses.R")
source("R/search_zipcode.R")
source("R/search_dob.R")
source("R/search_email.R")
source("R/search_phone.R")
source("R/search_ssn.R")

pii_table <- function(df) {
  df <- as.data.frame(df)
  class_table <- data.frame("Column" = colnames(df))
  
  for (i in 1:ncol(df)) {
    class_table$class[i] <- class(df[,i])
  }
  
  class_table <- class_table %>% filter(class == "character")
  df <- df[, c(class_table$Column)]
  risk_table <- data.frame("Column" = colnames(df))
  
  for (i in 1:ncol(df)) {
    risk_table$dobrisk[i] <- any(search_DOB(df[,i]))
    risk_table$emailrisk[i] <- any(search_email(df[,i]))
    risk_table$phonerisk[i] <- any(search_phone(df[,i]))
    risk_table$ssnrisk[i] <- any(search_SSN(df[,i]))
    #risk_table$addressrisk[i] <- any(search_addresses(df[,i]))
  }
  risk_table
}

pii_table(mtcars)
pii_table(band_instruments)
pii_table(ashley_madison)
