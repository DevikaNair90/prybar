#' Creates a summary table of PII for a datset.
#'
#' This function takes a dataframe and searches each column for presence of 
#' any text that appears as PII: emails, DOB, SSNs, phone numbers, and 
#' addresses (street address, city/states, zipcodes). 
#'
#' @param df A dataframe with (vector-able) variables.
#' @import stringr
#' @import dplyr
#' @suggest generator::
#' @export
#' @examples
#' set.seed(2)
#' testcase <- data.frame(name = generator::r_full_names(2),  
#' snn = generator::r_national_identification_numbers(2), 
#' dob = generator::r_date_of_births(2), 
#' email = generator::r_email_addresses(2), 
#' ip = generator::r_ipv4_addresses(2), 
#' phone = generator::r_phone_numbers(2),  
#' credit_card = generator::r_credit_card_numbers(2), 
#' lat = generator::r_latitudes(2), 
#' lon = generator::r_longitudes(2), 
#' stringsAsFactors = FALSE)
#' 
#' pii_table(testcase)
#' 
# devtools::load_all()
# devtools::use_package(package = "stringr", type = "import")
# devtools::use_package(package = "dplyr", type = "import")

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



