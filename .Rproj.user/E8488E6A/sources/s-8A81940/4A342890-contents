#' @title Save a single SOOSmap
#'
#' @author Kimberlee Baldry
#' @description This script contains a function to save a ggplot map using ggsave as a PNG file
#'
#' @note In development. Final draft.
#'
#' @param plot A ggplot object
#' @param WG_name The name of the working group associated with the plot
#' @param outdir The directory where the file should be saved
#'
#' @import ggplot2
#'
#' @export


save_map <- function(plot, WG_name, outdir){
  p_name = file.path(outdir, gsub(" ","",paste(WG_name, "_membermap_",format(Sys.Date(), "%Y"),".png", sep = "")))
  ggsave(p_name, plot =plot , width = 40, height = 25.8, units = "cm",  device = "png") # asp 1.550668
  print(paste(WG_name, "map has been saved as",basename(p_name), "in output directory!"))
}




