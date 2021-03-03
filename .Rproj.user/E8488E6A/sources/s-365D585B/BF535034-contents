#' @title Draw a single SOOS Working Group Map
#'
#' @author Kimberlee Baldry
#' @description This script contains a function to draw a map for a single working group. Markers will be added to the centroids of participating countries.
#'
#' @note v.0.1
#'
#' @return A ggplot object
#' @param WG_name Name of the WG to be plotted
#' @param WG_countries Character vector of the ISO3 codes of WG member countries
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

# for development/debugging
# WG_name = WG_names[rw]
# WG_countries = countries[[rw]]

plot_WG_map <- function(WG_name, WG_countries, participation_colour = SOOScol[1], country_border_colour = SOOScol[9], country_border_lwd = 1,
                        marker_colour = NULL, marker_dot_colour = NULL,
                        marker_size = 2.5, marker_asp = 1.1, marker_png = file.path(.libPaths()[1],"mapSOOSWG", "marker_base.png"),
                        marker_dot_png = file.path(.libPaths()[1],"mapSOOSWG", "marker_dot.png"),
                        add_marker = T, add_marker_dot = T, ...){


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

  ### Calculate centroids
  # Create a data from for the centroids of countries. This will allow up to put pins at locations
  wmap <- getMap(resolution="less islands")[-which(getMap()$ADMIN=="Antarctica"),]
  wmap = spTransform(wmap, "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  # get centroids
  centroids = gCentroid(wmap, byid=TRUE)
  centroids_df = as.data.frame(centroids)
  centroids_df = centroids_df[match(sub_Map$id[which(sub_Map$participation == 1)], rownames(centroids_df)),]

  ### Point to markers PNG files within the downloaded package folder
  centroids_df$image1  = marker_png
  if(add_marker_dot){centroids_df$image2  = marker_dot_png}

  ### Plot
  # Get baseplot
  baseWG = base_plot(...)

  # Add centroid markers and colour participating countries
  baseWG = baseWG +
    # plot SOOS WG countries (single colour)
    geom_polygon(data = map_data[participation == 1], aes(x = long, y = lat, group = group, ISO3 = ISO3), fill = participation_colour, colour = country_border_colour, lwd = country_border_lwd)

  # plot the markers. We plot the markers as 2 layers so that the marker centers always appear at the top and are not hidden.
  # add base marker
  if(add_marker){
    if(is.null(marker_colour)){
      baseWG = baseWG + geom_image(data = centroids_df, aes(x = x, y=y, image = image1), size = 0.01 * marker_size, asp = marker_asp)
    }else{baseWG = baseWG + geom_image(data = centroids_df, aes(x = x, y=y, image = image1), size = 0.01 * marker_size, asp = marker_asp, colour = marker_colour)}
  }
  # add dot
  if(add_marker_dot){
    if(is.null(marker_dot_colour)){
      baseWG = baseWG + geom_image(data = centroids_df, aes(x = x, y=y, image = image2), size = 0.01 * marker_size, asp = marker_asp)
    }else{baseWG = baseWG + geom_image(data = centroids_df, aes(x = x, y=y, image = image2), size = 0.01 * marker_size, asp = marker_asp, colour = marker_dot_colour)}
  }

  return(baseWG)

}
