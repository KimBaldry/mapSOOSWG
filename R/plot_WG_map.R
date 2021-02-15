#' @title Draw a single SOOS Working Group Map
#'
#' @author Kimberlee Baldry
#' @description This script contains a function to draw a map for a single working group.
#'
#' @note In development. Final Draft.
#'
#' @return A ggplot object
#' @param WG_name Name of the WG to be plotted
#' @param WG_countries Character vector of the ISO3 codes of WG member countries
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

plot_WG_map <- function(WG_name, WG_countries){

  ### plot options
  marker_size = 0.03 # marker size
  marker_asp = 1.1 # marker aspect ratio
  participation_colour = SOOScol[1] # colour of participating countries


  ### Create a map data frame that has an indicator for participating countries
  # This will allow us to colour countries
  # Note we remove Antarctica because it wont transform niceley
  countDF = data.frame(country = WG_countries,
                        participation = rep(1, length(WG_countries)))
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
  centroids_df$image  = file.path(.libPaths()[1],"mapSOOSWG", "marker3.png")
  centroids_df$image2  = file.path(.libPaths()[1],"mapSOOSWG", "marker_dot.png")

  ### Plot
  # Get baseplot
  baseWG = base_plot()

  # Add centroid markers and colour participating countries
  baseWG +
    # plot SOOS WG countries (single colour)
    geom_polygon(data =map_data[participation == 1], aes(x = long, y = lat, group = group), fill = participation_colour, colour = participation_colour) +

    # plot the markers. We plot the markers as 2 layers so that the marker centers always appear at the top and are not hidden.
    geom_image(data = centroids_df, aes(x = x, y=y, image = image), size = marker_size, asp = marker_asp)+
    geom_image(data = centroids_df, aes(x = x, y=y, image = image2), size = marker_size, asp = marker_asp)+


  return(baseWG)

}
