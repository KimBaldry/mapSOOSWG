#' @title Check and convert country names to ISO3
#'
#' @author Kimberlee Baldry
#' @description This function simply checks SOOS WG country data against the available lookup table. If the test is passed, it then converts SOOS country names into ISO3 codes for mapping.If the test is failed, a "Failed" message is returned.
#'
#' @note In development. Final draft.
#'
#' @return If check failed returns "Failed". If check sucessfull returns ISO3 country codes
#' @param countries list of country character vercors to be checked from the data file
#' @param lookupfile the filepath of the lookup file
#'
#' @import ggplot2
#' @import data.table
#' @import broom
#' @import rgeos
#' @import rworldmap
#' @import ggimage
#' @import tidyverse
#'
#' @export

check_country_names <- function(countries, lookupfile){
  # read the lookup file
  country_LT = read.csv(lookupfile, header = T, stringsAsFactors = F,strip.white = T)
  # unique list of countries
  unique_countries = unique(unlist(countries))
  # split by comma where an individual is listed with more than one country!
  unique_countries = unlist(strsplit(unique_countries,split = ","))
  unique_countries = trimws(unique_countries) # trim white space
  unique_countries = unique(unique_countries) # make sure unique

  # find countries that are not in the lookup table
  missing_countries = setdiff(unique_countries, country_LT$Country)
  # if all of the countries are there
  if(length(missing_countries) == 0){
    #change all names in countries to ISO3 codes and return
    check = lapply(countries, function(x){country_LT$ISO3_name[match(x, country_LT$Country)]})
  }else{
    print("Error: Cannot produce plots!")
    print(paste("Not all countries are in the lookupfile. Please check the following countries, which are missing in the file:",paste(missing_countries, collapse = ", ")))
    check = "Failed"
  }
  return(check)
}
