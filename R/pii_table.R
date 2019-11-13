#' Creates a summary table of PII for a dataset.
#'
#' This function takes a dataframe and searches each column for presence of 
#' any text that appears as PII: emails, DOB, SSNs, and phone numbers. 
#'
#' @param df A dataframe with (vector-able) variables.
#' @import stringr
#' @import dplyr
#' @suggest generator
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

pii_table <- function(df, path) {
  df_name <- deparse(substitute(df))
  df <- as.data.frame(df)
  class_table <- data.frame("Column" = colnames(df)) %>%
    mutate("Reference" = paste0(df_name, "$", Column))
  
  for (i in 1:ncol(df)) {
    class_table$class[i] <- class(df[,i])
  }
  
  # numvars <- class_table %>% filter(class %in% c("numeric", "integer", "double")) 
  # charvars <- class_table %>% filter(class == "character")
  # 
  # df_numbers <- df[, as.character(numvars$Column)]
  
  # if (is.data.frame(df_numbers) == FALSE) {
  #   df_numbers <- data.frame(NUM = df_numbers) 
  # }
  # 
  # columnsummary <- data.frame("Column" = colnames(df_numbers))
  
  df_email <- list(data.frame())
  df_ssn <- list(data.frame())
  df_dob <- list(data.frame())
  df_phone <- list(data.frame())
  #df_zipcode <- list(data.frame())
  
  for (i in 1:ncol(df)) {
    var <- as.character(df[,i])
    df_dob[[i]] <- search_DOB(var, "df")
    df_ssn[[i]] <- search_ssn(var, "df")
    df_email[[i]] <- search_email(var, "df")
    df_phone[[i]] <- search_phone(var, "df")
   # df_zipcode[[i]] <- search_zipcode(var, "df")
    class_table$dobrisk[i] <- sum(search_DOB(var), na.rm = TRUE)
    class_table$emailrisk[i] <- sum(search_email(var), na.rm = TRUE)
    class_table$phonerisk[i] <- sum(search_phone(var), na.rm = TRUE)
    class_table$ssnrisk[i] <- sum(search_ssn(var), na.rm = TRUE)
    
  }
  
  saveRDS(object = class_table, file = paste0(path, df_name, "_PII_0_Summary_", Sys.Date(), ".RDS")) 
  
  saveRDS(object = df_dob, file = paste0(path, df_name, "_PII_1_DOB_", Sys.Date(), ".RDS"))
  saveRDS(object = df_ssn, file = paste0(path, df_name, "_PII_2_SSN_", Sys.Date(), ".RDS"))
  saveRDS(object = df_email, file = paste0(path, df_name, "_PII_3_EMAIL_", Sys.Date(), ".RDS"))
  saveRDS(object = df_phone, file = paste0(path, df_name, "_PII_4_PHONE_", Sys.Date(), ".RDS"))
  
  class_table
  
}



