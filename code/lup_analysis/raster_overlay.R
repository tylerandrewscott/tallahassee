library (sp)
library (rgdal)
library(raster)
library(SDMTools)
library(adehabitatMA)
library (maps)
library (mapproj)
library(ModelMap)
#10702
# 0 Background,
# 1 Unclassified (Cloud, Shadow, etc),
# 2 High Intensity Developed,
# 3 Medium Intensity Developed,
# 4 Low Intensity Developed,
# 5 Developed Open Space,
# 6 Cultivated Land,
# 7 Pasture/Hay,
# 8 Grassland,
# 9 Deciduous Forest,
# 10 Evergreen Forest,
# 11 Mixed Forest,
# 12 Scrub/Shrub,
# 13 Palustrine Forested Wetland,
# 14 Palustrine Scrub/Shrub Wetland,
# 15 Palustrine Emergent Wetland,
# 16 Estuarine Forested Wetland,
# 17 Estuarine Scrub/Shrub Wetland,
# 18 Estuarine Emergent Wetland,
# 19 Unconsolidated Shore,
# 20 Bare Land,
# 21 Open Water,
# 22 Palustrine Aquatic Bed,
# 23 Estuarine Aquatic Bed,
# 24 Tundra,
# 25 Snow/Ice,

fl = readOGR('spatial_input/government_units/','county_nrcs_a_fl')
fl <- fl[fl@data$COUNTYNAME %in% c('Miami-Dade','Manatee','Hillsborough','Leon','St. Johns'),]
fl_96 <- raster('spatial_input/fl_1996_ccap_land_cover.img')
fl_01 <- raster('spatial_input/fl_2001_ccap_land_cover.img')
fl_06 <- raster('spatial_input/fl_2006_ccap_land_cover.img')
fl_10 <- raster('spatial_input/fl_2010_ccap_land_cover.img')
fl = spTransform(fl,CRS(proj4string(fl_96)))
class_matrix = rbind(c(0,1,NA),c(2,5,1),c(6,20,0),c(21,25,NA))
fl_96_dev <- reclassify(x = fl_96,rcl = class_matrix,right=NA)


fl_96_dev_agr4 <- aggregate(fl_96_dev,factor = 4,fun = modal,expand=F)
fl_96_dev_spdf = rasterToPoints(fl_96_dev_agr10,spatial=TRUE)
fl_96_dev_spdf <- fl_96_dev_spdf[fl,]


library(INLA)
require(maps)
### second way: using the location points
## suppose you have these points:
require(splancs)
### second way: using the location points
## suppose you have these points:
require(splancs)
pts <- csr(sp@polygons[[3]]@Polygons[[1]]@coords, 1e5)

require(INLA)
pt.bond <- inla.nonconvex.hull(coordinates(fl_96_dev_spdf), 2, 2)
pt.bond2 <- inla.nonconvex.hull(coordinates(fl_96_dev_spdf), 1, 1, 100)

mesh1 <- inla.mesh.2d(boundary=pt.bond, max.edge=c(5,10), cut=1)
mesh2 <- inla.mesh.2d(boundary=pt.bond, max.edge=c(3,7), cut=1)
mesh3 <- inla.mesh.2d(boundary=pt.bond2, max.edge=c(2,4), cut=1, off=c(1e-5,4))

c(mesh0$n, mesh1$n, mesh2$n, mesh3$n)

par(mfrow=c(2,2), mar=c(0,0,1,0))
plot(mesh0, asp=1)
plot(mesh1, asp=1)
plot(mesh2, asp=1)
plot(mesh3, asp=1)


require(INLA)
#mesh calculation
mesh<-inla.mesh.create.helper(points=coordinates(fl_96_dev_spdf),points.domain=grid[,1:2],offset=c(2,2,),max.edge=c(1,1,),min.angle=c(21,21)))
#SPDE object
spde<-inla.spde2.matern(mesh,alpha=2)
A.obs<-inla.spde.make.A(mesh,loc=obs_coords)
A.pred<-inla.spde.make.A(mesh,loc=grid_coords)




pt.bond <- inla.nonconvex.hull(fl_96_dev_spdf, 2, 2)
pt.bond2 <- inla.nonconvex.hull(fl_96_dev_spdf, 1, 1, 100)

mesh1 <- inla.mesh.2d(boundary=pt.bond, max.edge=c(5,10), cut=1)
mesh2 <- inla.mesh.2d(boundary=pt.bond, max.edge=c(3,7), cut=1)
mesh3 <- inla.mesh.2d(boundary=pt.bond2, max.edge=c(2,4), cut=1, off=c(1e-5,4))

c(mesh0$n, mesh1$n, mesh2$n, mesh3$n)

par(mfrow=c(2,2), mar=c(0,0,1,0))
plot(mesh0, asp=1)
plot(mesh1, asp=1)
plot(mesh2, asp=1)
plot(mesh3, asp=1)



IDs <- sapply(strsplit(us$names, ":"), function(x) x[1])

require(sp)
prj <- CRS("+proj=longlat +datum=WGS84")



### first way: using the polygon
require(INLA)

pl.bound <- inla.sp2segment(fl_96_dev_spdf)
mesh0 <- inla.mesh.2d(boundary=pl.bound, max.edge=c(3,7), cutoff=1)




fl_01_dev <- reclassify(x = fl_01,rcl = class_matrix,right=NA)
fl_06_dev <- reclassify(x = fl_06,rcl = class_matrix,right=NA)
fl_10_dev <- reclassify(x = fl_10,rcl = class_matrix,right=NA)




fl_01_dev <- aggregate(fl_01_dev,factor = 100,fun = modal,expand=F)
fl_06_dev <- aggregate(fl_06_dev,factor = 100,fun = modal,expand=F)
fl_10_dev <- aggregate(fl_10_dev,factor = 100,fun = modal,expand=F)



fl_01_dev_spdf = rasterToPoints(fl_01_dev,spatial=TRUE)
fl_01_dev_spdf <- fl_01_dev_spdf[fl,]
fl_06_dev_spdf = rasterToPoints(fl_06_dev,spatial=TRUE)
fl_06_dev_spdf <- fl_06_dev_spdf[fl,]
fl_10_dev_spdf = rasterToPoints(fl_10_dev,spatial=TRUE)
fl_10_dev_spdf <- fl_10_dev_spdf[fl,]

save.image('scratch_spatial/fl_100factor.RData')


#max.edge.length <- c(25, 40)
#loc1 <- as.matrix(coordinates(fl_96_dev_spdf))
#mesh <- inla.mesh.2d(loc=loc1, max.edge = max.edge.length, offset=1, cutoff = 5)





