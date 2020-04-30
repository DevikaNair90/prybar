#' Creates a summary table of PII geography for a dataset.
#'
#' This function takes a dataframe and searches each column for presence of 
#' any text that appears as PII geography: street address, city/states, zipcodes. 
#'
#' @param df A dataframe with (vector-able) variables.
#' @import stringr
#' @import data.table
#' @import maditr
#' @suggest generator
#' @export
#' @examples
#' fakeaddresses <- c("820 Nut Swamp Ave.
#' Toms River, NJ 08753",
#'                    "982 Clay Street
#'                    Lenoir, NC 28645",
#'                    "4 NW. Mayfield Rd.
#'                    Springfield Gardens, NY 11413",
#'                    "20 Elmwood Street
#'                    Raleigh, NC 27603",
#'                    "196 E. Green Lake Road
#'                    Birmingham, AL 35209",
#'                    "73 Beechwood Dr.
#'                    La Crosse, WI VA DC 54601")
#' 
#' pii_geo_table(fakeaddresses)
#' 
# devtools::load_all()
# devtools::use_package(package = "stringr", type = "import")
# devtools::use_package(package = "dplyr", type = "import")

pii_geo_table <- function(df, path, writeout) {
  print(Sys.time())
  df_name <- deparse(substitute(df))
  df <- data.table::as.data.table(df)
  class_table <- data.table::data.table("Column" = colnames(df)) %>%
    dt_mutate("Reference" = paste0(df_name, "$", Column))
  
  for (i in 1:ncol(df)) {
    class_table$class[i] <- class(df[[i]])
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
  
  df_state <- list(data.table())
  df_city <- list(data.table())
  df_zipcode <- list(data.table())
  df_street <- list(data.table())
  
  # STATES #####################################################################################
  
  for (i in 1:ncol(df)) {
    var <- as.character(df[[i]])
    df_state[[i]] <- search_state(var, "dt")
    class_table$staterisk[i] <- sum(df_state[[i]]$StatesYN, na.rm = TRUE)
  }
  
  if (missing(writeout)||writeout == TRUE) {
    saveRDS(object = df_state, file = paste0(path, df_name, "_PII_1_STATES_", Sys.Date(), ".RDS"))
  }
  
  rm(df_state)
  print("states completed")
  print(Sys.time())
  
  # CITIES #####################################################################################
  
  for (i in 1:ncol(df)) {
    var <- as.character(df[[i]])
    df_city[[i]] <- search_cities_in_states(var, "dt")
    class_table$cityrisk[i] <- sum(df_city[[i]]$CitiesYN, na.rm = TRUE)
  }
  
  if (missing(writeout)||writeout == TRUE) {
    saveRDS(object = df_city, file = paste0(path, df_name, "_PII_2_CITIES_", Sys.Date(), ".RDS"))
  }
  
  rm(df_city)
  print("cities completed")
  print(Sys.time())
  
  # ZIPCODES #####################################################################################
  
  for (i in 1:ncol(df)) {
    var <- as.character(df[[i]])
    df_zipcode[[i]] <- search_zipcode(var, "dt")
    class_table$zipcoderisk[i] <- sum(df_zipcode[[i]]$ZipCodeYN, na.rm = TRUE)
  }
  
  if (missing(writeout)||writeout == TRUE) {
    saveRDS(object = df_zipcode, file = paste0(path, df_name, "_PII_3_ZIPCODES_", Sys.Date(), ".RDS"))
  }
  
  rm(df_zipcode)
  print("zipcodes completed")
  print(Sys.time())
  
  # STREET #####################################################################################
  
  # for (i in 1:ncol(df)) {
  #   var <- as.character(df[,i])
  #   df_street[[i]] <- search_streets(var, "df")
  #   class_table$streetrisk[i] <- sum(df_street[[i]]$StreetMention, na.rm = TRUE)
  # }
  # 
  # if (missing(writeout)||writeout == TRUE) {
  #   saveRDS(object = df_street, file = paste0(path, df_name, "_PII_4_STREETS_", Sys.Date(), ".RDS"))
  # }
  # 
  # rm(df_street)
  # print("streets completed")
  # print(Sys.time())
  # 
  if (missing(writeout)||writeout == TRUE) {
  saveRDS(object = class_table, file = paste0(path, df_name, "_PII_0_Summary_", Sys.Date(), ".RDS"))
  }

  print("summary completed")
  print(Sys.time())
  
  class_table
  
}



