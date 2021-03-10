####SET SIZE -- meters between hexagon center points
size <- 5000
library(sp)
library(rgdal)
library(raster)
library(SDMTools)
library(maps)
library(rgeos)
library(mapproj)
library(ModelMap)
library(pbapply)
library(tidyverse)
library(magrittr)
library(dplyr)
library(viridis);library(ggthemes)
library(spatialEco)
library(parallel)
library(INLA)
fl = readOGR('spatial_input/government_units/','county_nrcs_a_fl')
fl_ccaps <- lapply(grep('fl_[0-9]{4}_ccap',list.files('spatial_input/'),value=T),function(x) raster(paste0('spatial_input/',x)))
fl_ccap_stack <- do.call(stack,fl_ccaps)
fl = spTransform(fl,CRS(proj4string(fl_ccap_stack)))
fl@data$COUNTYNAME = gsub('\\.','',fl@data$COUNTYNAME)
hex_points <- spsample(fl, type = "hexagonal", cellsize = size,offset=c(0,0))
hex_grid <- HexPoints2SpatialPolygons(hex_points, dx = size)
hex_rasters <- pblapply(1:length(hex_grid),function(x) trim(mask(crop(fl_ccap_stack,hex_grid[x,]),hex_grid[x,])),cl=24)
hex_points <- pblapply(hex_rasters, rasterToPoints,spatial=TRUE,cl = 24)


hex_points_spdf <- Reduce(rbind,hex_points)

writeOGR(cdds, "spatial_input/created_inputs", "ccap_hex4km_points", driver="ESRI Shapefile",overwrite_layer = TRUE)

