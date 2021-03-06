% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/search_email.R
\name{search_email}
\alias{search_email}
\title{Searches vector for emails.}
\usage{
search_email(vec, output)
}
\arguments{
\item{vec}{A vector input whose contents need to be searched for references
to emails}

\item{output}{The desired output of function. Defaults to "vector" where T/F 
vector result is returned. The argument "dt" will output a table of original 
vector input, T/F vector result, and the matching substring.}
}
\description{
This function takes a vector input and searches presence of any text that 
appears to be an email.
}
\details{
The default output is a TRUE/FALSE vector but the function can also
return a dataframe of the original vector input, the TRUE/FALSE result, 
and the substring that matched the email text pattern.
}
\examples{
set.seed(2)
testcase <- data.frame(name = generator::r_full_names(2),  
snn = generator::r_national_identification_numbers(2), 
dob = generator::r_date_of_births(2), 
email = generator::r_email_addresses(2), 
ip = generator::r_ipv4_addresses(2), 
phone = generator::r_phone_numbers(2),  
credit_card = generator::r_credit_card_numbers(2), 
lat = generator::r_latitudes(2), 
lon = generator::r_longitudes(2), 
stringsAsFactors = FALSE)

search_email(testcase$email)
}
