#' \code{prybar} package
#' 
#' description text
#'
#' @docType package
#' @name prybar

NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
utils::globalVariables(c(".", "Column", "StreetsYN", "CitiesYN", "ZipCodeYN", 
                         "StatesYN", "ID", "OriginalString", "StatesString", 
                         "stringsearchbefore", "pattern", "CitiesYN", "CitiesString"))

# utils::globalVariables(c("Column", ".", "StreetsYN"))