% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_country_names.R
\name{check_country_names}
\alias{check_country_names}
\title{Check and convert country names to ISO3}
\usage{
check_country_names(countries, country_names_data)
}
\arguments{
\item{countries}{list of country character vercors to be checked from the data file}

\item{country_names_data}{A data frame. The data frame must contain 2 columns labeled "Country" (what SOOS records the country as) and "ISO3_name". See "example_country_data" for an example.}
}
\value{
If check failed returns "Failed". If check sucessfull returns a list of data frames with ISO3 country codes and participation numbers.
}
\description{
This function simply checks SOOS WG country data against the available lookup table. If the test is passed, it then converts SOOS country names into ISO3 codes for mapping and returns a data frame with countries and the number of participants (count).If the test is failed, a "Failed" message is returned.
}
\note{
v.0.1
}
\author{
Kimberlee Baldry
}
