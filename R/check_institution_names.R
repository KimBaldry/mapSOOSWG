#' @title Check and get geocodes of institutions
#'
#' @author Kimberlee Baldry
#' @description This function simply checks SOOS institution data against the available geocode table. If the test is passed, it then retrieves lat, lon coordinates for SOOS institution names as a data frame. If the test is failed, a "Failed" message is returned.
#'
#' @note In development. Editing.
#'
#' @return If check failed returns "Failed". If check sucessfull returns institution,lat,lon as a datafram
#' @param institutions list of institution character vercors to be checked from the data file
#' @param geocodefile the filepath of the geocode file
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

check_institution_names <- function(institutions, geocodefile){
  # read the lookup file
  institution_LT = read.csv(geocodefile, header = T, stringsAsFactors = F,strip.white = T)
  # unique list of institutions
  unique_institutions = unique(unlist(institutions))
  # split by comma where an individual is listed with more than one institution!
  unique_institutions = unlist(strsplit(unique_institutions,split = ","))
  unique_institutions = trimws(unique_institutions) # trim white space
  unique_institutions = unique(unique_institutions) # make sure unique

  # find institutions that are not in the lookup table
  missing_institutions = setdiff(unique_institutions, institution_LT$Institution)
  # if all of the institutions are there
  if(length(missing_institutions) == 0){
    #change all names in institutions to ISO3 codes and return
    check = lapply(institutions, function(x){institution_LT[match(x, institution_LT$Institution),]})
  }else{
    print("Error: Cannot produce plots!")
    print(paste("Not all institutions are in the geocode file. Please check the following institutions, which are missing in the file:",paste(missing_institutions, collapse = ", ")))
    check = "Failed"
  }
  return(check)
}
