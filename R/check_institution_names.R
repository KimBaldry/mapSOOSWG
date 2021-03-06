#' @title Check and get geocodes of institutions
#'
#' @author Kimberlee Baldry
#' @description This function simply checks SOOS institution data against the available geocode table. If the test is passed, it then retrieves lat, lon coordinates for SOOS institution names as a data frame. If the test is failed, a "Failed" message is returned.
#'
#' @note v.0.1
#'
#' @return If check failed returns "Failed". If check sucessfull returns institution,lat,lon and a participation count as a dataframe
#' @param institutions list of institution character vercors to be checked from the data file
#' @param institution_names_data A data frame. The data frame must contain 4 columns labeled "Name" (what SOOS records the Affiliations as), "Type", "Latitude" and "Longitude". SSee example_institution_data for an example.#'
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

check_institution_names <- function(institutions, institution_names_data){

  ### add semicolon to separate institutions
  plot_df = list()
  inst_clean = list()
  for(t in 1:length(institutions)){

    for(x in institution_names_data$Name){
      # test that the institution name from the lookup table appears
      test = grepl(x, institutions[[t]], fixed = T)
      if(any(test)){
        # if the name appears, add a semi colon after the name
        idx = which(test)
        for(i in idx){
          institutions[[t]][i] = gsub("\"","", institutions[[t]][i],fixed = T)
          institutions[[t]][i] = gsub(x,paste(x, ";", sep = ""), institutions[[t]][i],fixed = T)
          }
      }
    }
    # loop through the listed institutions for a working group and "clean" the names by splitting with the new ";," delimiter
    for(i in 1:length(institutions[[t]])){
      if(i == 1){inst_clean[[t]] = trimws(gsub(";","",unlist(strsplit(institutions[[t]][[i]],split = ";,"))))}else{
      inst_clean[[t]] = c(inst_clean[[t]], trimws(gsub(";","",unlist(strsplit(institutions[[t]][[i]],split = ";,")))))}
    }
    # create a data frame for each WG with latitudes and longitude
    plot_df[[t]] = data.frame("inst" = inst_clean[[t]], "lat" = as.numeric(institution_names_data$Latitude[match(inst_clean[[t]], institution_names_data$Name)]), "lon" =  as.numeric(institution_names_data$Longitude[match(inst_clean[[t]], institution_names_data$Name)]))
    # count number of participants and aggregate
    plot_df[[t]] = plot_df[[t]] %>% group_by(inst, lat, lon) %>% mutate(count = n())
    # replace institution list
    institutions[[t]] = plot_df[[t]]$inst
    # identify entries with no coordinates FYI
    if(t == 1){no_coord = as.character(plot_df[[t]]$inst[which(is.na(plot_df[[t]]$lat))])}else{
    no_coord = c(no_coord, as.character(plot_df[[t]]$inst[which(is.na(plot_df[[t]]$lat))]))}
    plot_df[[t]] = plot_df[[t]] %>% filter(is.finite(lat))
  }


  # unique list of institutions
  unique_institutions = unique(unlist(institutions))

  # find institutions that are not in the lookup table
  missing_institutions = setdiff(unique_institutions, institution_names_data$Name)

  # if all of the institutions are there
  if(length(missing_institutions) == 0){
    # return the data frame to plot markers
    if(length(no_coord) > 0){print(paste("Warning! The following institutions have no coordinates. They have been removed:", paste(unique(no_coord) , collapse = ","), sep = " "))}
    check = plot_df
  }else{
    # if not, give information about problems
    print("Error: Cannot produce plots!")
    print(paste("Not all institutions are in the geocode file. Please check the following institutions, which are missing in the file:",paste(missing_institutions, collapse = ", ")))
    check = "Failed"
  }

  return(check)
}
