test = read_csv('input/sprawl_dev_indices_version2.csv')

plot(test[,grepl('developed_area_',names(test))])
summary(test %>% dplyr::select(-X1) )
rmask <- matrix(c(1,1,1,1,1,
                  1,1,1,1,1,
                  1,1,0,1,1,
                  1,1,1,1,1,
                  1,1,1,1,1),nrow=5)

raster = raster('spatial_input/County_CCAPS/fl_2006_ccap_land_cover_12086.grd')

focalFun <- function(x) { return(sum(x == 0,na.rm=T) / sum(x != 1,na.rm=T))}

plot(raster)
developed <- reclassify(raster, cbind(c(c(2:5),c(0:1),c(6:30)),
                                      c(rep(1,length(2:5)),rep(NA,length(0:1)),rep(0,length(6:30)))))
developed[developed == 0] <- NA		
develUndevelAndWater <- reclassify(raster,cbind(c(2:5,21:23,0:1,6:20),
                                                c(rep(2,length(2:5)),rep(1,length(21:23)),rep(NA,length(0:1)),rep(0,length(6:20)))))
dryland <- reclassify(raster, cbind(c(c(21:23),c(1:20)),c(rep(0,length(21:23)),rep(NA,length(0:1)),rep(1,length(2:20)))))
surroundingRatio <- focal(develUndevelAndWater, w=rmask, fun=focalFun)
  sprawl <- surroundingRatio * developed
  sprawlT <- trim(sprawl)
  developedT <- crop(developed, sprawlT)
  sprawlIndex <- cellStats(sprawl, stat="sum")/freq(developedT, value=1,useNA = 'no')

library(ggplot2)
library(ggthemes)

g1 = gplot(develUndevelAndWater) + 
  geom_tile(aes(fill=factor(value))) + scale_fill_manual(name = 'Classification',labels = c('Natural area','Water','Development',''),
                                                         values = c('dark green','blue','grey',NA)) + theme_map() + ggtitle('Land cover type - Miami-Dade County 2006')

g1
surroundingRatio[develUndevelAndWater==1] <- NA
library(viridis)

g2 = gplot(surroundingRatio) + 
  geom_tile(aes(fill=value)) + scale_fill_viridis(name = 'Surrounding ratio - generalized index') + theme_map() + 
  ggtitle('Generalized sprawl measure - Miami-Dade County 2006')
g2

surroundingRatio[is.na(developed)] <- NA
g3 = gplot(surroundingRatio) + 
  geom_tile(aes(fill=value)) + scale_fill_viridis(name = 'Surrounding ratio - generalized index') + theme_map() + 
  ggtitle('Generalized sprawl measure - Miami-Dade County 2006') +
  geom_path(data = fortify(md),aes(x = long,y = lat,group =group))
g3

focalFun <- function(x) { return(sum(x == 2,na.rm=T) / sum(x != 1,na.rm=T)) }
surroundingRatio2 <- focal(develUndevelAndWater, w=rmask, fun=focalFun)
surroundingRatio2[is.na(developed)] <- NA

g4 = gplot(surroundingRatio2) + 
  geom_tile(aes(fill=value)) + scale_fill_viridis(name = 'Surrounding ratio - inverted index') + theme_map() + 
  ggtitle('Inverted sprawl measure - Miami-Dade County 2006') +
  geom_path(data = fortify(md),aes(x = long,y = lat,group =group))
g4

focalFun <- function(x) {
  return((sum(x == 2,na.rm=T) * 0.9 
          + sum(x == 3,na.rm=T) * .65 
          + sum(x == 4,na.rm=T) * 0.35 
          + sum(x == 5,na.rm=T) * 0.1) 
         / sum(!(x %in% 21:23),na.rm=T))}
surroundingRatio3 <- focal(raster, w=rmask, fun=focalFun)
surroundingRatio3[is.na(developed)] <- NA

g5 = gplot(surroundingRatio3) + 
  geom_tile(aes(fill=value)) + scale_fill_viridis(name = 'Surrounding ratio - intensity index') + theme_map() + 
  ggtitle('Intensity sprawl measure - Miami-Dade County 2006') +
  geom_path(data = fortify(md),aes(x = long,y = lat,group =group))
g5
library(ggmap)
ggmap(get_map('Miami-Dade County',maptype = 'satellite',zoom = 9))
sprawl <- surroundingRatio * developed
sprawlT <- trim(sprawl)
developedT <- crop(developed, sprawlT)
sprawlIndex <- cellStats(sprawl, stat="sum")/freq(developedT, value=1,useNA = 'no')


new.extent
extent(md)
extent@.Data()
str(extent)

new.extent
extent(md) * 0.6
extent(md)
plot(us.pop.logged, ext = new.extent)
plot(developed,add=T,col = 'red')
?plot
extent(md)

gplot(dryland) +  
  geom_tile(aes(fill=factor(value))) + scale_fill_manual(labels = c('water','land'),values = c('light blue','dark green')) +
  theme_map() + scale_y_continuous(expand=c(0,0)) + scale_x_continuous(expand=c(0,0)) + theme(legend.title =element_blank(),legend.position = c(0.1,0.1),
                                                                                              title = element_text(size=16)) + 
  ggtitle('Miami-Dade County land and water')
  
geom_polygon(data=OR, aes(x=long, y=lat, group=group), 
               fill=NA,color="grey50", size=1)+
  coord_equal()

ggplot() + geom_raster(dryland)  
?geom_raster
plot(dryland, legend = FALSE, col = c('blue','green'))
  
  
  terrain.colors(4)
library(rasterVis)
levelplot(dryland,col.regions=rev(terrain.colors(2)), xlab="", ylab="")
plot(surroundingRatio)
test = surroundingRatio * dryland
plot(dryland,discrete=T)
?raster::plot

plot(surroundingRatio[!is.na(dryland)])
fl_sub$match(fl_sub$FIPS_C, test$FIPS[test$Year==2006])
head(fl_sub@data,8)
dataplot(test$developed_area_intensity~test$developed_area_generalized)
plot(test$sprawl_index_intensity~test$sprawl_index_generalized)
plot(test$sprawl_index_intensity~test$sprawl_index_inverted)

test$sprawl_index_inverted
