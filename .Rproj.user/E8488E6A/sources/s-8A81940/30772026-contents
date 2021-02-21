
base_plot <- function(){

  ### plot options
  background_colour = SOOScol[3]
  world_map_colour = SOOScol[4]
  border_colour = "black"
  border_thickness = 1


  ### Define plot area
  # needs some consideration to change
  # have adjusted for a cropped anatarctica - antarctica will need to be recropped and reproduced using the script "create_antarctica_shape.R"
  # note this extra script is in the files I gave you, NOT within the package
  plot_area = c(-20037508, 20037508, -32493690 + 15000000, 18397474 + 5E+05) # c(xmin, xmax, ymin, ymax)

  ### Get basemap data
  wmap <- getMap(resolution="less islands")[-which(getMap()$ADMIN=="Antarctica"),]
  wmap = spTransform(wmap, "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  wmap = tidy(wmap)

  ## baseplot
  baseWO = ggplot() +
    # plot background box
    geom_rect(aes(xmin = plot_area[1],xmax = plot_area[2], ymin = plot_area[3], ymax = plot_area[4]),size = 1, color = NA, fill = background_colour)+
    # plot whole world
    geom_polygon(data =A, aes(x = long, y = lat, group = group),fill = world_map_colour, colour = world_map_colour) +
    # plot antarctica. Antarctica is hard! This uses "ant" from the package data
    geom_sf(data = ant,fill = SOOScol[4], colour = SOOScol[4]) +

    # plot the box border
    geom_rect(aes(xmin = plot_area[1],xmax = plot_area[2], ymin = plot_area[3], ymax = plot_area[4]),size = border_thickness, color = border_colour, fill = NA)+
    xlab("") +
    ylab("") +
    # Change theme to remove axes and ticks
    theme(panel.background = element_blank(),legend.position = "none",
          panel.grid.minor = element_blank(),axis.ticks=element_blank(), axis.text = element_blank())

  return(baseWO)
}
