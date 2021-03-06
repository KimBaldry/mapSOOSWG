#' @title Draw a single SOOS Working Group Map with institution markers
#'
#' @author Kimberlee Baldry
#' @description This script contains a function to draw a map for a single working group with institution markers.
#'
#' @note v.0.1
#'
#' @return A ggplot object
#' @param WG_name Name of the WG to be plotted
#' @param WG_institutions Dataframe of the institution geocodes to be plotted with column names as "Institution", "lat" and "lon"
#' @param hilight_countries If true, also colour the member countries.
#' @param WG_countries Data frame including ISO3 codes of WG member countries. Output from check_country_names()
#' @param participation_colour The fill colour of the participating countries.
#' @param country_border_colour The colour of the participating country borders.
#' @param country_border_lwd The width of the participating country borders.
#' @param marker_colour The colour of the marker base, if not inherited from the PNG file. Leave as NULL if the PNG file contains the right colour. Note that no outline is possible if not drawn in the PNG file.
#' @param marker_dot_colour The colour of the marker dot, if not inherited from the PNG file. Leave as NULL if the PNG file contains the right colour.
#' @param marker_size The size of the marker.
#' @param marker_asp The apect ratio to adjust the marker.
#' @param marker_png The pathe to the base marker PNG file.
#' @param marker_dot_png The path to the marker dot PNG file.
#' @param add_marker If FALSE, no markers will be added.
#' @param add_marker_dot TRUE of a marker dot is being added to the plot.
#' @param ... see base_plot() for further plot options (background_colour, world_map_colour, border_colour, plot_border_thickness)
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


plot_geocode_map <- function(WG_name, WG_institutions, hilight_countries, WG_countries = NULL, participation_colour = SOOScol[1], country_border_colour = SOOScol[9], country_border_lwd = 1,
                             marker_colour = NULL, marker_dot_colour = NULL,
                             marker_size = 2.5, marker_asp = 1.1, marker_png = file.path(.libPaths()[1],"mapSOOSWG", "marker_base.png"),
                             marker_dot_png = file.path(.libPaths()[1],"mapSOOSWG", "marker_dot.png"),
                             add_marker = T, add_marker_dot = T, ...){

  if(hilight_countries){
    ### Create a map data frame that has an indicator for participating countries
    # This will allow us to colour countries
    # Note we remove Antarctica because it wont transform niceley
    countDF = data.frame(country = WG_countries$ISO3,
                         participation = rep(1, nrow(WG_countries)))
    Map = joinCountryData2Map(countDF, joinCode = "ISO3",
                              nameJoinColumn = "country")[-which(getMap()$ADMIN=="Antarctica"),]
    # Transofrm to Mercator projection
    Map = spTransform(Map, "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    map_data = tidy(Map)
    # attach map data for plotting
    sub_Map =Map@data[,c("ADMIN","ISO3","participation")]
    colnames(sub_Map)[1] = c("id")
    map_data = data.table(left_join(map_data, sub_Map))
  }

  ### reproject institution data
  in_df = WG_institutions
  coordinates(in_df) =  ~lon+lat
  projection(in_df) = "+proj=longlat +datum=WGS84"
  in_df_merc = spTransform(in_df, "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  in_df_merc = as.data.frame(in_df_merc)

  ### Point to markers PNG files within the downloaded package folder
  in_df_merc$image1  = marker_png
  if(add_marker_dot){in_df_merc$image2  = marker_dot_png}

  ### Plot
  # Get baseplot
  baseIN = base_plot(...)

  # Colour participating countries
  if(hilight_countries){
  baseIN = baseIN +
    # plot SOOS WG countries (single colour)
    geom_polygon(data =map_data[participation == 1], aes(x = long, y = lat, group = group, ISO3 = ISO3), fill = participation_colour, colour = country_border_colour, lwd = country_border_lwd)
  }

  # plot the markers. We plot the markers as 2 layers so that the marker centers always appear at the top and are not hidden.
  # add base marker
  if(add_marker){
    if(is.null(marker_colour)){
      baseIN = baseIN + geom_image(data = in_df_merc, aes(x = lon, y=lat, image = image1), size = 0.01 * marker_size, asp = marker_asp)
    }else{baseIN = baseIN + geom_image(data = in_df_merc, aes(x = lon, y=lat, image = image1), size = 0.01 * marker_size, asp = marker_asp, colour = marker_colour)}
  }
  # add dot
  if(add_marker_dot){
    if(is.null(marker_dot_colour)){
      baseIN = baseIN + geom_image(data = in_df_merc, aes(x = lon, y=lat, image = image2), size = 0.01 * marker_size, asp = marker_asp)
    }else{baseIN = baseIN + geom_image(data = in_df_merc, aes(x = lon, y=lat, image = image2), size = 0.01 * marker_size, asp = marker_asp, colour = marker_dot_colour)}
  }

  return(baseIN)

}
