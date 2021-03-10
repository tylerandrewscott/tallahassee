bay1 = readOGR('spatial_input/county_cdd/bay_county_subs','LakePowellCCD')
bay2 = readOGR('spatial_input/county_cdd/bay_county_subs','PierParkCCD')
bay3 = readOGR('spatial_input/county_cdd/bay_county_subs','SeahavenCCD')
bay3 = gUnaryUnion(bay3, id = rep(1,3))
cdd_bay = do.call(bind,list(bay1,bay2,bay3))
cdd_bay = spTransform(cdd_bay,CRS(proj4string(fl)))
cdd_bay@data$CDD_NAME = c('Lake Powell Residential Golf Community Development District',
                          'Pier Park Community Develpment District',
                          'Towne of Seahaven Community Development District')
cdd_bay@data$COUNTY = 'bay'
cdd_bay@data$Id = 1:length(cdd_bay)
writeOGR(cdd_bay, "spatial_input/county_cdd", "FL_Bay_CDD", driver="ESRI Shapefile",overwrite_layer = TRUE)

