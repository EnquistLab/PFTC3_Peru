### Coordinates
library("sf")
library("tidyverse")
library("WriteXLS")

# Load shapefile
coords <- read_sf('~/Dropbox/Data/CoordsTransects/coords.shp')

WriteXLS(coords, "coordinates.xls")
  