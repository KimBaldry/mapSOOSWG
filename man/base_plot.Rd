% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/base_plot.R
\name{base_plot}
\alias{base_plot}
\title{The baseplot of SOOS Working Group maps}
\usage{
base_plot(
  background_colour = SOOScol[3],
  world_map_colour = SOOScol[4],
  border_colour = "black",
  plot_border_thickness = 1
)
}
\arguments{
\item{background_colour}{Background colour}

\item{world_map_colour}{Colour of the world map}

\item{border_colour}{Border colour of the plot}

\item{plot_border_thickness}{Thickness of the plot border}
}
\value{
A ggplot object
}
\description{
This script contains a function to draw a basemap for use in all plotting functions.
}
\note{
v.0.1
}
\author{
Kimberlee Baldry
}
