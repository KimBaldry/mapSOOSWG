#' @title Clean category data frame to help plot new country fill colours
#'
#' @author Kimberlee Baldry
#' @description This script contains a function to create a data frame to help reassign participating country fill in a ggplot object created from plot_WG_map() or plot_geocode_map().
#'
#' @return a data frame with the columns "ISO3" (ISO3 ccountry code), "cat" (category ID) and "fill" (assigned fill colour)
#' @param cat_data_orig same as category_data in category_maps(). This data frame must contain 3 columns labeled "Acronym" (for the WG or plotname), "Countries" (A list of SOOS named countries separated by a ";") and "Categories" (a list of assigned categories for each SOOS country). See example file for details. Note the categories should be ordered from low-high and can be numbers or a caracter vector corresponding to names in the "Categories" column. The highest ranking category will be decide the country fill.
#' @param cat_countries_ISO3 ISO3 codes returned by passing cat_data_original through check_country_names().
#' @param cols The ordered colour vector for categories.
#' @param WG_i The row of cat_data_orig that data is being generated for.
#'
#' @note v.0.1
#'
#' @import data.table
#' @import broom
#' @import tidyverse
#'
country_categories <- function(cat_data_orig, cat_countries_ISO3, cols, WG_i){

  ### add category information
  cat_df = str_split(cat_data_orig[WG_i,(2:3)], ";")
  cat_df = data.frame("ISO3" = cat_countries_ISO3[[WG_i]]$ISO3, "cat" = as.numeric(cat_df[[2]]), "count" = cat_countries_ISO3[[WG_i]]$count)
  n = length(unique(cat_df$cat))
  # order low-high
  cat_df = cat_df[order(cat_df$cat),]
  # only keep the highest category for a country
  cat_df = cat_df[!duplicated(cat_df$ISO3),]
  # match up fill values
  cat_df$fill = cols[match(cat_df$cat,1:length(cols))]

  return(cat_df)
  }
