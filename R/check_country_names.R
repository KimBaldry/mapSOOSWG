#' @title Check and convert country names to ISO3
#'
#' @author Kimberlee Baldry
#' @description This function simply checks SOOS WG country data against the available lookup table. If the test is passed, it then converts SOOS country names into ISO3 codes for mapping and returns a data frame with countries and the number of participants (count).If the test is failed, a "Failed" message is returned.
#'
#' @note v.0.1
#'
#' @return If check failed returns "Failed". If check sucessfull returns a list of data frames with ISO3 country codes and participation numbers.
#' @param countries list of country character vercors to be checked from the data file
#' @param country_names_data A data frame. The data frame must contain 2 columns labeled "Country" (what SOOS records the country as) and "ISO3_name". See "example_country_data" for an example.
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

check_country_names <- function(countries, country_names_data){

  # unique list of countries
  unique_countries = unique(unlist(countries))
  # split by comma where an individual is listed with more than one country!
  unique_countries = unlist(strsplit(unique_countries,split = ","))
  unique_countries = trimws(unique_countries) # trim white space
  unique_countries = unique(unique_countries) # make sure unique

  # find countries that are not in the lookup table
  missing_countries = setdiff(unique_countries, country_names_data$Country)
  # change all names in countries to ISO3 codes
  countries_ed = countries
  for(e in 1:length(countries)){
    countries_ed[[e]] = unlist(strsplit(unlist(countries_ed[[e]]),split = ","))
    countries_ed[[e]] = trimws(countries_ed[[e]])
    #countries_ed[[e]] = gsub("United States ", "United States", countries_ed[[e]])
  }
  ISO3 = lapply(countries_ed, function(x){country_names_data$ISO3_name[match(x, country_names_data$Country)]})

  # if all of the countries are there
  if(length(missing_countries) == 0){
    # count participants with ISO3 codes
    plot_df = list()
    for(t in 1:length(ISO3)){
      plot_df[[t]] = data.frame("ISO3" = ISO3[[t]])
      # count number of participants and aggregate
      plot_df[[t]] = plot_df[[t]] %>% group_by(ISO3) %>% mutate(count = n())
    }
    # and return
    check = plot_df
  }else{
    # give information about problems
    print("Error: Cannot produce plots!")
    print(paste("Not all countries are in the lookupfile. Please check the following countries, which are missing in the file:",paste(missing_countries, collapse = ", ")))
    check = "Failed"
  }
  return(check)
}
