#' @title Create multiple SOOS Working Group Map from a file
#'
#' @author Kimberlee Baldry
#' @description This script contains a function to create all SOOS WG maps from a file. The function: - acesses the file - applies data from rows of the file to 1. plot a WG map using plot_WG_map() and 2. save the file as a .png file using save_map() - outputs a message for each sucessfull map and when teh run is complete
#'
#' @note In development. Final Draft.
#'
#' @return It will let you know if the code worked!
#' @param SOOS_WG_data A data frame. The data frame must contain the columns named "Acronym" (for the WG) and "Countries.Represented". See "example_WG_data" for an example.
#' @param outdir The directory where you would like .png files saved
#' @param country_names_data A data frame. The data frame must contain 2 columns labeled "Country" (what SOOS records the country as) and "ISO3_name". See "example_country_data" for an example.
#' @param ... see base_plot() and plot_WG_map() for further plot options (background_colour, world_map_colour, border_colour, plot_border_thickness)
#'
#' @import ggplot2
#' @import data.table
#' @import broom
#' @import rgeos
#' @import rworldmap
#' @import ggimage
#' @import tidyverse
#' @import raster
#' @import sf
#'
#' @export

WG_maps <- function(SOOS_WG_data, outdir, country_names_data, ...){

  # prepare data
  WG_names = SOOS_WG_data$Acronym
  countries = lapply(SOOS_WG_data$Countries.Represented, FUN = function(x){unlist(strsplit(x, split = "; "))})

  # check that all countries to be plotted appear in the country lookup table
  check = check_country_names(countries, country_names_data)
  if(any(check == "Failed")){break}
  countries = check
  # loop through WG
  for(rw in 1:length(WG_names)){
    # make plot object
    map = plot_WG_map(WG_names[rw], countries[[rw]], ...)
    # save plot
    save_map(map,WG_names[rw], outdir)
  }
  print("Sucess! All maps were created")
}
