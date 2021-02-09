#' @title Draw a single SOOS Working Group Map
#'
#' @author Kimberlee Baldry
#' @description This script contains a function to draw a map for a single working group.
#'
#' @note In development
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
#'
#' @export

# for development/debugging
# WG_name = WG_names[rw]
# WG_countries = countries[[rw]]

plot_WG_map_style2 <- function(WG_name, WG_countries){

  SOOScol = c("#00aeef", "#0073ae","#f1f3f4" ,"#c5c0bb", "#8dc63f")
  # Create a data frame that has an indicator for participating countries
  # This will allow us to colour countries
  countDF = data.frame(country = WG_countries,
                       participation = rep(1, length(WG_countries)))
  Map = joinCountryData2Map(countDF, joinCode = "ISO3",
                            nameJoinColumn = "country")[-which(getMap()$ADMIN=="Antarctica"),]
  Map = spTransform(Map, "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  map_data = tidy(Map)
  sub_Map =Map@data[,c("ADMIN","ISO3","participation")]
  colnames(sub_Map)[1] = c("id")
  map_data = data.table(left_join(map_data, sub_Map))

  # Create a data from for the centroids of countries. This will allow up to put pins at locations
  wmap <- getMap(resolution="less islands")[-which(getMap()$ADMIN=="Antarctica"),]
  wmap = spTransform(wmap, "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  ext_wmap = extent(wmap)
  buffer = 5E+05
  ext_wmap[c(1,3)] = ext_wmap[c(1,3)] - buffer
  ext_wmap[c(2,4)] = ext_wmap[c(2,4)] + buffer
  # get centroids
  centroids = gCentroid(wmap, byid=TRUE)
  centroids_df = as.data.frame(centroids)
  centroids_df = centroids_df[match(sub_Map$id[which(sub_Map$participation == 1)], rownames(centroids_df)),]

  centroids_df$image  = file.path(.libPaths()[1],"mapSOOSWG", "marker3.png")

  # This is the baseplot code for the map
  baseWO =  ggplot() +
    geom_rect(aes(xmin = ext_wmap[1],xmax = ext_wmap[2], ymin = ext_wmap[3], ymax = ext_wmap[4]),size = 1, color = "black", fill = SOOScol[3])+
    geom_polygon(data =map_data, aes(x = long, y = lat, group = group),fill = SOOScol[4], colour = SOOScol[4]) +
    # we just plot red polygons over the top to avoid having to use scale_fill twice
    geom_polygon(data =map_data[participation == 1], aes(x = long, y = lat, group = group), fill = SOOScol[1], colour = SOOScol[1], lwd = 0.5) +
    xlab("") +
    ylab("") +
    #geom_point(data = df, aes(x=x, y=y), col = colours[5], cex = 3) +
    geom_image(data = centroids_df, aes(x = x, y=y, image = image), size = 0.02, asp =1.5)+ # add asp (aspect ratio width:height). Add nudge
    geom_rect(aes(xmin = ext_wmap[1],xmax = ext_wmap[2], ymin = ext_wmap[3], ymax = ext_wmap[4]),size = 1, color = "black", fill = NA)+

    # Adds axes
    # Change theme to remove axes and ticks
    theme(panel.background = element_blank(),legend.position = "none",
          panel.grid.minor = element_blank(),axis.ticks=element_blank(), axis.text = element_blank())

  return(baseWO)

}
