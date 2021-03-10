

library(tidyverse)
library(rgdal)
library(maptools)
library(rgeos)
library(raster)
library(maps)
library(SDMTools)
library(doParallel)  #Foreach Parallel Adaptor 
library(foreach)     #Provides foreach looping construct
library(pbapply)
rasterOptions(maxmemory = 1e+09)
counties = readOGR('spatial_input/government_units/','tl_2016_us_county')
counties@data$Square_Miles = {as.numeric(as.character(counties@data$ALAND))+
    as.numeric(as.character(counties@data$AWATER))} * 0.00000038610
counties@data$FIPS = paste0(counties@data$STATEFP,counties@data$COUNTYFP)
counties@data$STATE_FIPS = str_extract(counties@data$FIPS,'^[0-9]{2}')
state.fips$fips <- sprintf("%02i",as.numeric(state.fips$fips))
counties = counties[counties@data$STATEFP %in% state.fips$fips,]
counties@data$STATE <- state.fips$abb[match(counties@data$STATEFP,state.fips$fips)]

UseCores=10
cl <- makeCluster(UseCores)
registerDoParallel(cl)

foreach(i = 1:nrow(counties@data)) %dopar% {
library(raster)
pref = 'spatial_input/State_CCAPS/'
fls <- list.files(pref,pattern = paste0('^',tolower(counties@data$STATE[i]),'_'))
fls <- fls[grepl('ccap_land_cover',fls)]
if(length(fls)!=0){
for (j in 1:length(fls))
{
temp = raster(paste0(pref,fls[j])) 
tcounty = spTransform(counties[i,],CRS(proj4string(temp)))
temp = crop(temp,tcounty)
temp = mask(temp,tcounty)
raster::writeRaster(x = temp,filename = paste0('spatial_input/County_CCAPS/',gsub('_[0-9]{1,}$','',gsub('\\.img','',fls[j])),'_',counties@data$FIPS[i],'.grd'),
                                               overwrite=T)
}}
}
stopCluster(cl)


