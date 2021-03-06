#' @title Save a single SOOSmap
#'
#' @author Kimberlee Baldry
#' @description This script contains a function to save a ggplot map using ggsave as a PNG file
#'
#' @note v.0.1
#'
#' @param plot A ggplot object
#' @param WG_name The name of the working group associated with the plot
#' @param outdir The directory where the file should be saved
#' @param p_width plot width in cm
#' @param p_height plot height in cm
#'
#' @import ggplot2
#'
#' @export

save_map <- function(plot, WG_name, outdir, p_width = 35, p_height = 25.8){
  p_name = file.path(outdir, gsub(" ","",paste(WG_name, "_membermap_",format(Sys.Date(), "%Y"),".png", sep = "")))
  ggsave(p_name, plot =plot , width = p_width, height = p_height, units = "cm",  device = "png") # asp 1.550668
  print(paste(WG_name, "map has been saved as",basename(p_name), "in output directory!"))
}




