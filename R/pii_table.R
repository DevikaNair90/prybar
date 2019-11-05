library(dplyr)
library(stringr)

setwd("Documents/GitHub/privaR/")

source("R/search_addresses.R")
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
  
  #class_table <- class_table %>% filter(class == "character")
  df <- df[, c(class_table$Column)]
  risk_table <- data.frame("Column" = colnames(df))
  
  for (i in 1:ncol(df)) {
    var <- as.character(df[,i])
    risk_table$dobrisk[i] <- any(search_DOB(var))
    risk_table$emailrisk[i] <- any(search_email(var))
    risk_table$phonerisk[i] <- any(search_phone(var))
    risk_table$ssnrisk[i] <- any(search_SSN(var))
    risk_table$addressrisk[i] <- any(search_addresses(var))
  }
  risk_table
}



