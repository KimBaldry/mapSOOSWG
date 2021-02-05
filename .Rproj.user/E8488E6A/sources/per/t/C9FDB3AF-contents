#' @title Create multiple SOOS Working Group Map from a file
#'
#' @author Kimberlee Baldry
#' @description This script contains a function to create all SOOS WG maps from a file. The function: - acesses the file - applies data from rows of the file to 1. plot a WG map using plot_WG_map() and 2. save the file as a .png file using save_map() - outputs a message for each sucessfull map and when teh run is complete
#'
#' @note In development
#'
#' @return A ggplot object
#' @param filepath The fullname of the file
#' @param outdir The directory where you would like .png files saved
#'
#' @export

# for development/debugging
# maindir = "C:/Users/kabaldry/OneDrive - University of Tasmania/Documents/SOOS"
# filename = file.path(maindir, "data", "exampleWGlist.txt")
# outdir = file.path(maindir, "output")
# lookupfile = file.path(maindir, "data", "country_lookup_table.txt")

WG_maps <- function(filename, outdir, lookupfile){

  # read the file
  data = read.table(filename, header = T, sep = "\t", stringsAsFactors = F)
  print(paste(basename(filename), "sucessfully opened"))
  # prepare data
  WG_names = data[,1]
  countries = lapply(data[,2], FUN = function(x){unlist(strsplit(x, split = "; "))})
  # check that all countries to be plotted appear in the country lookup table
  check = check_country_names(countries, lookupfile)
  if(any(check == "Failed")){break}
  countries = check
  # loop through WG
  for(rw in 1:length(WG_names)){
    # make plot object
    map = plot_WG_map(WG_names[rw], countries[[rw]])
    # save plot
    save_map(map,WG_names[rw], outdir)
  }
  print("Sucess! All maps were created")
}
