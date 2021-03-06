#' @title Create multiple SOOS Mapd from a file with extra category data
#'
#' @author Kimberlee Baldry
#' @description This script contains a function to create all SOOS WG maps from a file. The function: - acesses the file - applies data from rows of the file to 1. plot a WG map using plot_WG_map() and 2. save the file as a .png file using save_map() - outputs a message for each sucessfull map and when teh run is complete
#'
#' @note v.0.1. Note the appearance of the legend still needs some fiddling.
#'
#' @return It will let you know if the code worked!
#' @param SOOS_WG_data A data frame. The data frame must contain the columns named "Acronym" (for the WG) and "Countries.Represented". See "example_WG_data" for an example. Needed for plot_type "WG" and "geocode". MUST have the same ordered rows as "category_data" !!
#' @param outdir The directory where you would like .png files saved
#' @param category_data This data frame must contain 3 columns labeled "Acronym" (for the WG or plotname), "Countries" (A list of SOOS named countries separated by a ";") and "Categories" (a list of assigned categories for each SOOS country). See example file for details. Note the categories should be ordered from low-high and can be numbers or a caracter vector corresponding to names in the "Categories" column. The highest ranking category will be decide the country fill. Can also be SOOS_WG_data
#' @param country_names_data A data frame. The data frame must contain 2 columns labeled "Country" (what SOOS records the country as) and "ISO3_name". See "example_country_data" for an example.
#' @param plot_type Is the baseplot a WG plot (plot_type = "WG"), a geocode plot (plot_type = "geocode"), or are there no markers (plot_type = "category_only")?
#' @param category_colours The category colours, in order from lowest to highests category.
#' @param institution_names_data A data frame. The data frame must contain 4 columns labeled "Name" (what SOOS records the Affiliations as), "Type", "Latitude" and "Longitude". SSee example_institution_data for an example.
#' @param category_values A vector containing ordered (low-high) category values if they are not numbered (Leave blank if numbered, starting from 1)
#' @param plot_legend Plot a legend for categories?
#' @param category_names The names you want to appear on the plot legend.
#' @param category_legend The legend title.
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
#' @import gridExtra
#'
#' @export

category_maps <- function(SOOS_WG_data = NULL, outdir, category_data, country_names_data, plot_type = "category_only", category_colours = SOOScol,
                          category_values = NULL, plot_legend = T, category_names = NULL, category_legend = NULL,
                          institution_names_data = NULL,...){

  ### Convert category names to numbers
  if(!is.null(category_values)){
    for(i in 1:length(category_values)){
      category_data$Categories = gsub(category_values[i],i,category_data$Categories)
    }
  }

  # identify the highest category number
  n_max = max(as.numeric(unlist(strsplit(category_data$Categories, ";"))))

  ### Load category information
  # note all variables starting with "cat" relate to the category data
  cat_WG_names = category_data$Acronym
  cat_countries = lapply(category_data$Countries.Represented, FUN = function(x){unlist(strsplit(x, split = "; "))})

  # check all country names appear in the lokup table (country_names_data) and convert to ISO3
  check = check_country_names(cat_countries, country_names_data)
  if(any(check == "Failed")){break}
  # return ISO3 codes
  cat_countries = check

  ### Additional data if plotting markers

  # dont actually need this code.... Assume we only want markers for centroids of countries with asigned categories
  # keep for later maybe... if marker countries are different to category countries.
  # if(plot_type == "WG"){
  #   ### WG country data
  #   WG_names = SOOS_WG_data$Acronym
  #   countries = lapply(SOOS_WG_data$Countries.Represented, FUN = function(x){unlist(strsplit(x, split = "; "))})
  #
  #   # check all country names appear in the lokup table (country_names_data) and convert to ISO3
  #   check = check_country_names(countries, country_names_data)
  #   if(any(check == "Failed")){break}
  #   # return ISO3 codes
  #   countries = check
  # }

  if(plot_type == "geocode"){
    ### get institution geocoded data
    inst = lapply(SOOS_WG_data$Affiliations, FUN = function(x){unlist(strsplit(x, split = "; "))})
    # check that all institutions to be plotted appear in the lookup table (institution_names_data)
    check = check_institution_names(inst, institution_names_data)
    if(any(check == "Failed")){break}
    # return data frame with geocodes
    inst_data = check
  }


  ### Construct plots for each WG
  for(rw in 1:length(cat_WG_names)){

    # get category data for adjusting the fill of base plots
    cat_df = country_categories(category_data, cat_countries, category_colours, rw)
    # chop the length of colours to match the highest number of categories
    category_colours = category_colours[1:n_max]

    if(plot_type == "category_only"){
      # use plot_WG_map without markers
      baseCAT = plot_WG_map(cat_WG_names[rw], cat_countries[[rw]], add_marker = F, add_marker_dot = F)
    }

    ### make plot object
    if(plot_type == "WG"){
      # use plot_WG_map. Note that the country data comes from category_data NOT SOOS_WG_data
      baseCAT = plot_WG_map(cat_WG_names[rw], cat_countries[[rw]])
    }

    if(plot_type == "geocode"){
      # use plot_geocode_map. Don't hilight contries, they will just be re-filled anyway. Makes it a little faster.
      baseCAT = plot_geocode_map(WG_name = WG_names[rw], WG_institutions = inst_data[[rw]], WG_countries = cat_countries[[rw]], hilight_countries = F)
    }


    ### adjust fill based on category data
    temp = ggplot_build(baseCAT)
    temp$data[[5]]$fill = cat_df$fill[match(temp$data[[5]]$ISO3,cat_df$ISO3)]
    temp_ggtable = ggplot_gtable(temp)

    if(plot_legend){
      # if there are no category_names set to numbers for legend label
      if(is.null(category_names)){category_names =  seq(1, n_max)}

      ### add a legend
      # This is where you can edit the legend size and style
      if(rw == 1){
        # create a dummy plot
        dummy = ggplot(temp$data[[5]], aes(x = x, y = y, group = group, fill  = fill)) + geom_polygon() +
          scale_fill_manual(name = category_legend, labels = category_names,  guide = "legend", values = category_colours)+
            #guides(name = category_legend, fill = guide_legend(labels = category_names))+
            theme(legend.position = "right", legend.key.size = unit(4,"line"), legend.text = element_text(size = 18))
        # select the legend gtable from the dummy plot
        g = ggplotGrob(dummy)$grobs
        legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
      }
      ### EDIT legend appearance below. may be a better way to do this.
      # combine the legend and the category plot
      # the layout matrix for the combined plot
      lay = rbind(c(1,NA),c(1,2), c(1,2), c(1,2), c(1,NA))
      # adjust the width of the legend here for longer names
      CAT = grid.arrange(grobs = list(temp_ggtable, legend),layout_matrix = lay, widths = c(6,1))
      }else{
      CAT = temp_ggtable
    }

    ### save plot
    save_map(CAT,cat_WG_names[rw], outdir)
  }
  print("Sucess! All maps were created")
}
