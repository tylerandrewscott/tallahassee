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
library(sf)
library(tigris)

fl = st_read('spatial_input/government_units/county_nrcs_a_fl.shp')
fl_ccaps <- lapply(grep('fl_[0-9]{4}_ccap',list.files('spatial_input/'),value=T),function(x) raster(paste0('spatial_input/',x)))
fl_ccap_stack <- stack(fl_ccaps)
fl = st_transform(fl,proj4string(fl_ccap_stack))
fl$COUNTYNAME = gsub('\\.','',fl$COUNTYNAME)
fl$CFIPS = paste0('12',fl$FIPSCO)
county_cdd_info <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vRUcfsL-5FxeSsaIz3WFa_6DveJexTj4iDd2UUB_Y3hTAch5vCqJAhiNycPsCVnaw9LKI0HNOEU3oAo/pub?gid=0&single=true&output=csv')
county_cdd_info$CFIPS = as.character(county_cdd_info$FIPS)

keep_counties = county_cdd_info$CFIPS[
  {!is.na(county_cdd_info$Have_Shapefiles) & county_cdd_info$Have_Shapefiles==1 & county_cdd_info$IN_CBSA == 1} |
  {is.na(county_cdd_info$Have_Shapefiles) & county_cdd_info$IN_CBSA == 1}]
fl_sub <- fl[fl$CFIPS %in% keep_counties,]

fl_df <- left_join(fl,county_cdd_info)
county_cdd_info$FIPS
library(ggthemes)
library(albersusa)

fl_df$CDD_COUNT_CBSA = ifelse(fl_df$IN_CBSA ==1,fl_df$CDD_COUNT,NA)
spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
TA <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +datum=NAD83 +units=m +ellps=GRS80 +towgs84=0,0,0") 


figure_1 <- ggplot() + ggtitle('CDDs observed for counties in core-base statistical areas') + 
  geom_sf(data = fl_df,aes(fill = CDD_COUNT_CBSA),colour = 'grey50') +
  theme_map() +
  scale_fill_viridis(name = '# of CDDs',na.value='white') +
theme(panel.grid = element_blank(),legend.position = c(0.3,0.2),legend.text = element_text(size = 14),legend.title = element_text(size = 14),title = element_text(size = 14),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()) + coord_sf(datum=NA)
figure_1

#gArea(fl_sub)/gArea(fl)
size <- 7000
fl_sub_sp = as(fl_sub,'Spatial')
hex_points <- spsample(fl_sub_sp, type = "hexagonal", cellsize = size,offset=c(0,0))
hex_grid <- HexPoints2SpatialPolygons(hex_points, dx = size)

rasterOptions(maxmemory = 1e+09)
gArea(fl_sub)/gArea(fl)
rmask <- matrix(c(1,1,1,1,1,
                  1,1,1,1,1,
                  1,1,0,1,1,
                  1,1,1,1,1,
                  1,1,1,1,1),nrow=5)
library(rasterVis)
source('code/sprawlIndex.R')   
source('code/devArea.R')

h = length(hex_grid)
hex_grid_sf = sf::st_as_sf(hex_grid)
hex_rasters <- pblapply(1:h,function(x) trim(mask(crop(fl_ccap_stack,hex_grid_sf[x,]),hex_grid_sf[x,])),cl=24)
hex_generalized_sprawl_index <- pblapply(1:h,function(x) sapply(names(hex_rasters[[x]]), function(r) sprawl_index_function(hex_rasters[[x]][[r]],mask = rmask,functionType = 'generalized')),cl = 24)
hex_inverted_sprawl_index <- pblapply(1:h,function(x) sapply(names(hex_rasters[[x]]), function(r) sprawl_index_function(hex_rasters[[x]][[r]],mask = rmask,functionType = 'inverted')),cl = 24)
hex_intensity_sprawl_index <- pblapply(1:h,function(x) sapply(names(hex_rasters[[x]]), function(r) sprawl_index_function(hex_rasters[[x]][[r]],mask = rmask,functionType = 'intensity')),cl = 24)

#save.image('scratch.RData')

hex_generalized_sprawl_df <- do.call(rbind,hex_generalized_sprawl_index) %>% as.data.frame() %>% mutate(hex_id = 1:h) %>% gather(year,value,-hex_id) %>% mutate(var = 'generalized_sprawl_index')
hex_inverted_sprawl_df <- do.call(rbind,hex_inverted_sprawl_index) %>% as.data.frame() %>% mutate(hex_id = 1:h) %>% gather(year,value,-hex_id) %>% mutate(var = 'inverted_sprawl_index')
hex_intensity_sprawl_df <- do.call(rbind,hex_intensity_sprawl_index) %>% as.data.frame() %>% mutate(hex_id = 1:h) %>% gather(year,value,-hex_id) %>% mutate(var = 'intensity_sprawl_index')

hex_sprawl_df = Reduce(full_join,lapply(grep('^hex_.*sprawl_df$',ls(),value=T),get))
hex_sprawl_df$value <- as.numeric(hex_sprawl_df$value)

source('code/devArea.R')
dev_area_generalized <- pblapply(hex_rasters[1:h],function(x) sapply(names(x), function(r) dev_function(x[[r]],functionType =  'generalized')),cl = 24)
dev_area_low_intensity <- pblapply(hex_rasters[1:h],function(x) sapply(names(x), function(r) dev_function(x[[r]],functionType =  'low_intensity')),cl = 24)
dev_area_intensity <- pblapply(hex_rasters[1:h],function(x) sapply(names(x), function(r) dev_function(x[[r]],functionType =  'intensity')),cl = 24)

dev_area_generalized_df <- do.call(rbind,dev_area_generalized) %>% as.data.frame() %>% mutate(hex_id = 1:h) %>% gather(year,value,-hex_id) %>% mutate(var = 'dev_area_generalized')
dev_area_intensity_df <- do.call(rbind,dev_area_intensity) %>% as.data.frame() %>% mutate(hex_id = 1:h) %>% gather(year,value,-hex_id) %>% mutate(var = 'dev_area_intensity')
dev_area_low_intensity_df <- do.call(rbind,dev_area_low_intensity) %>% as.data.frame() %>% mutate(hex_id = 1:h) %>% gather(year,value,-hex_id) %>% mutate(var = 'dev_area_low_intensity')
dev_area_df = Reduce(f = dplyr::full_join,lapply(grep('dev_area_.*_df$',ls(),value=T),get))  

hex_df = full_join(hex_sprawl_df,dev_area_df) %>% 
  mutate(year = str_extract(year,'[0-9]{4}')) %>% spread(var,value)

fl_places <- readOGR('spatial_input/government_units','tl_2016_12_place')
fl_places <- fl_places[!grepl('CDP',fl_places@data$NAMELSAD),]
fl_places = spTransform(fl_places,CRS(proj4string(fl_ccap_stack)))
fl_places = fl_places[fl_sub,]

cdds <- readOGR('spatial_input/created_inputs','master_cdd_dataset')
cdds <- spTransform(cdds,CRS(proj4string(hex_grid)))
sp::spChFIDs(cdds) <- paste0('R',1:length(cdds))
cdds = gBuffer(cdds,byid=TRUE, width=0)
cdds@data = cdds@data %>% rename(Year_Created = Yr_Crtd,Year_Dissolved = Yr_Dctv,CDD_NAME = CDD_NAM)

library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(rgeos)
library(cleangeo)
library(pbapply)
library(cleangeo)
base_area <- sapply(1:length(hex_grid),function(x) gArea(hex_grid[x,]))

#hex_df$year = as.numeric(hex_df$year)
over_cdd_zone = pblapply(1:nrow(hex_df),function(x) gIntersection(hex_grid[hex_df$hex_id[x],],cdds[(!is.na(cdds@data$Year_Created)) &cdds@data$Year_Created<=hex_df$year[x],]),cl = 24)
over_cdd_area =  pbsapply(over_cdd_zone,function(x) ifelse(is.null(x),0,gArea(x)))
hex_df$prop_over_cdd <- over_cdd_area/base_area

fl_places = gBuffer(fl_places,byid=TRUE, width=0)
#any_place = over(hex_grid,fl_places,returnList = F)
over_place_zone = pblapply(1:length(hex_grid),function(x) gIntersection(hex_grid[x,],fl_places),cl = 24)
over_place_area =  pbsapply(over_place_zone,function(x) ifelse(is.null(x),0,gArea(x)))
hex_df$prop_over_place <- over_place_area/base_area

conserv_fl = readOGR('spatial_input/','flma_201709')
conserv_fl = spTransform(conserv_fl, CRS(proj4string(fl_sub)))
conserv_fl_state_fed = conserv_fl[grepl('USDA|^L,F-U|BLM|USFWS|NPS',conserv_fl@data$OWNERTYPES)|
                                    grepl('FL Fish and Wildlife|FL Dept. of Environmental Protection|Div. of Recreation and Parks',conserv_fl@data$MANAGING_A),]
conserv_fl_state_fed = conserv_fl_state_fed[fl_sub,]
conserv_fl_state_fed = gBuffer(conserv_fl_state_fed,byid = T, width=0)
over_conserv_zone = pblapply(1:length(hex_grid),function(x) gIntersection(hex_grid[x,],conserv_fl_state_fed),cl = 24)
over_conserv_area =  pbsapply(over_conserv_zone,function(x) ifelse(is.null(x),0,gArea(x)))
hex_df$prop_over_conserv <- over_conserv_area/base_area

spChFIDs(hex_grid) <- 1:length(hex_grid)
library(spdep)
hex_nb = poly2nb(hex_grid,queen=FALSE)
hex_adj = nb2mat(hex_nb,style = 'B',zero.policy = TRUE)
hex_adj = as(hex_adj,'dgTMatrix')

library("INLA")
hex_df$idx = hex_df$hex_id
which_county = over(hex_grid,fl_sub)
hex_df$county = which_county$COUNTYNAME[hex_df$hex_id]

zcta = readOGR('spatial_input/government_units/','cb_2016_us_zcta510_500k')
zcta = spTransform(zcta,CRS(proj4string(fl_sub)))
zip_homes = read_csv('input/Zip_MedianValuePerSqft_AllHomes.csv') %>% filter(State=='FL')
hex_zcta = over(hex_grid,zcta,returnList = T)
pers = c('1996-04','2001-01','2006-01','2010-01')
hex_df = hex_df %>% mutate(yi = ifelse(year == 1996,1,ifelse(year == 2001,2,ifelse(year == 2006,3,4))))

hex_df$ZCTA_Median_pcsqft = pbsapply(1:nrow(hex_df), 
function(i) colMeans(as.vector(zip_homes[match(hex_zcta[[hex_df$hex_id[i]]]$ZCTA5CE10,zip_homes$RegionName),][pers[hex_df$yi[i]]]),na.rm=T))
zc_fl = zcta[fl,]
zc_fl@data = left_join(zc_fl@data %>% mutate(ZCID = as.character(ZCTA5CE10)),
                       zip_homes %>% mutate(ZCID = as.character(RegionName),
                                            "1996" = `1996-04`,"2001" = `2001-01`,"2006" = `2006-01`,"2010" = `2010-01`) %>% 
                         select(ZCID,`1996`,`2001`,`2006`,`2010`))

zc_fl_nona = zc_fl[!is.na(zc_fl$`1996`),]

hex_centroids = coordinates(gCentroid(hex_grid,byid = T))
zc_fl_centroids = coordinates(gCentroid(zc_fl_nona,byid=T))
library(spdep)
four_nearest_zctas = Reduce(full_join,pblapply(1:nrow(hex_centroids),function(i) data.frame(neigh = knearneigh(rbind(hex_centroids[i,],zc_fl_centroids),k = 4)$nn[1,],ID = i)))
hex_df$avg_four_nearest_mpsqft = sapply(1:nrow(hex_df),function(i)
  mean(zc_fl_nona@data[four_nearest_zctas$neigh[four_nearest_zctas$ID == hex_df$hex_id[i]],
                       as.character(hex_df$year[i])]))
hex_df$ZCTA_Median_pcsqft[is.na(hex_df$ZCTA_Median_pcsqft)] <- hex_df$avg_four_nearest_mpsqft[is.na(hex_df$ZCTA_Median_pcsqft)]

library(INLA)

#save.image('scratch/scratch_model_start.RData')
hex_df <- hex_df %>% group_by(year) %>% 
  mutate(period_standardized_pcsqft = as.vector(scale(ZCTA_Median_pcsqft)))
hex_df$intensity_reversed_sprawl_index <- -hex_df$intensity_sprawl_index

vars = grep('sprawl_index|dev_area|period_standardized',names(hex_df),value=T )

#hex_df <- hex_df[,!grepl('last|change',names(hex_df))]
hex_df <- hex_df %>% arrange(hex_id,year) %>% 
  group_by(hex_id) %>% 
  mutate_at(vars,funs(change = . - lag(.),last = lag(.))) 
#save.image('scratch/premod_results.RData')

dvs <- grep('intensity_reversed_sprawl_index_change|dev_area_intensity_change|dev_area_generalized_change|generalized_sprawl_index_change',names(hex_df),value=T)

draw_base_forms <- list("~ 1 + dev_area_intensity_last + 
  prop_over_place + prop_over_conserv + 
  period_standardized_pcsqft_change +  f(county,model = 'iid') + f(year,model = 'iid') +
  f(idx, model = 'besag', graph = hex_adj,scale.model = TRUE)",
                        
  "~ 1 + intensity_sprawl_index_last + dev_area_intensity_last + 
  prop_over_place + prop_over_conserv + 
  period_standardized_pcsqft_change + f(county,model = 'iid') + f(year,model = 'iid') +
  f(idx, model = 'besag', graph = hex_adj,scale.model = TRUE)")

base_forms <- ifelse(grepl('dev_area',dvs),list(draw_base_forms[[1]]),list(draw_base_forms[[2]]))
names(base_forms) <- dvs

base_mods <- lapply(dvs,function(x) 
  inla(as.formula(paste0(x,base_forms[x])), data = hex_df[hex_df$year!=1996,], 
       control.predictor = list(compute = TRUE),control.compute = list(cpo=TRUE,dic=TRUE,waic=TRUE)))
names(base_mods) <- paste(dvs,'base',sep='_')

draw_full_forms <- list("~ 1 + dev_area_intensity_last + 
  prop_over_place + prop_over_conserv + 
  period_standardized_pcsqft_change + prop_over_cdd + f(county,model = 'iid') + f(year,model = 'iid') +
  f(idx, model = 'besag', graph = hex_adj,scale.model = TRUE)",
"~ 1 + intensity_sprawl_index_last + dev_area_intensity_last + 
  prop_over_place + prop_over_conserv + 
  period_standardized_pcsqft_change + prop_over_cdd + 
f(county,model = 'iid') + f(year,model = 'iid') +
  f(idx, model = 'besag', graph = hex_adj,scale.model = TRUE)")
full_forms <- ifelse(grepl('dev_area',dvs),list(draw_full_forms[[1]]),list(draw_full_forms[[2]]))
names(full_forms) <- dvs

full_mods <- lapply(dvs,function(x) 
  inla(as.formula(paste0(x,full_forms[x])), data = hex_df[hex_df$year!=1996,], 
       control.predictor = list(compute = TRUE),control.compute = list(cpo=TRUE,dic=TRUE,waic=TRUE))
)
names(full_mods) <- paste(dvs,'full',sep='_')

full_sub_mods <- lapply(dvs,function(x) 
  inla(as.formula(paste0(x,full_forms[x])), data = hex_df[hex_df$year!=1996 & hex_df$dev_area_generalized_last>0.01,], 
       control.predictor = list(compute = TRUE),control.compute = list(cpo=TRUE,dic=TRUE,waic=TRUE))
)
names(full_sub_mods) <- paste(dvs,'full_sub',sep='_')



base_results = Reduce(full_join,lapply(1:length(base_mods),function(x) 
  base_mods[[x]]$summary.fixed[,c(1,3,5)] %>% cbind(.,dev = dvs[x],mod = names(base_mods)[x]) %>%
                                         mutate(coef = rownames(.)))) 
full_results = Reduce(full_join,lapply(1:length(full_mods),function(x) 
  full_mods[[x]]$summary.fixed[,c(1,3,5)] %>% cbind(.,dev = dvs[x],mod = names(full_mods)[x]) %>%
    mutate(coef = rownames(.)))) 
full_sub_results = Reduce(full_join,lapply(1:length(full_sub_mods),function(x) 
  full_sub_mods[[x]]$summary.fixed[,c(1,3,5)] %>% cbind(.,dev = dvs[x],mod = names(full_sub_mods)[x]) %>%
    mutate(coef = rownames(.)))) 


res_table = Reduce(full_join,list(base_results,full_results,full_sub_results))  %>% dplyr::select(-mean) %>% 
  mutate(CI = paste(sprintf("%.3f",round(`0.025quant`,3)),sprintf("%.3f",round(`0.975quant`,3)),sep=', ')) %>%
dplyr::select(-dev,-`0.025quant`,-`0.975quant`) %>% spread(mod,CI)


ics <- rbind(do.call(rbind,lapply(1:length(base_mods),function(x) cbind(mod = names(base_mods)[x],dic = base_mods[[x]]$dic$dic,
                                             waic = base_mods[[x]]$waic$waic))),
do.call(rbind,lapply(1:length(full_mods),function(x) cbind(mod = names(full_mods)[x],dic = full_mods[[x]]$dic$dic,
                                                           waic = full_mods[[x]]$waic$waic))),
do.call(rbind,lapply(1:length(full_sub_mods),function(x) cbind(mod = names(full_sub_mods)[x],dic = full_sub_mods[[x]]$dic$dic,
                                                           waic = full_sub_mods[[x]]$waic$waic))))

ics = data.frame(ics) %>% mutate(dic = sprintf("%.3f",round(as.numeric(as.character(dic)),3)),waic = sprintf("%.3f",round(as.numeric(as.character(waic)),3)))
ics_df <- data.frame(rbind(ics %>% dplyr::select(-waic) %>% spread(mod,dic),
ics %>% dplyr::select(-dic) %>% spread(mod,waic)),coef = c('dic','waic'))


rt = full_join(res_table,ics_df)

library(stargazer)

#save.image('scratch/mod_results.RData')
#rm(list=ls())
#load('scratch/mod_results.RData')
#stargazer::stargazer(rt[c(1,2,3,4,6,7,5,8,9),c('coef','dev_area_intensity_change_base','dev_area_intensity_change_full',
#                                    'intensity_reversed_sprawl_index_change_base','intensity_reversed_sprawl_index_change_full')],
#         summary=F,out = 'scratch/result_table.html')


#####plotting examples######
#make sample plot

hills = fl_sub[fl_sub$COUNTYNAME=='Hillsborough',]
sf_grid = st_as_sf(hex_grid)
sf_grid$id = 1:nrow(sf_grid)

hplaces = gIntersection(fl_places,hills)
hconserv = gIntersection(conserv_fl_state_fed,hills)
hcdd = gIntersection(cdds,hills)
hills_sp = as(hills,'Spatial') 

ggplot() + geom_sf(data = hills)
figure_3 <- ggplot() +
  #geom_polygon(data=fortify(hills) %>% filter(lat>600000),aes(x=long,y=lat,group=group),fill = 'grey95',col = 'grey60') +
  geom_path(data = fortify(hills_sp) %>% filter(lat>600000),aes(x=long,y=lat,group=group)) + 
  geom_polygon(data=fortify(hplaces)%>% filter(lat>600000),aes(x=long,y=lat,group=group,fill = 'city')) +
  geom_polygon(data=fortify(hcdd)%>% filter(lat>600000),aes(x=long,y=lat,group=group,fill = 'cdd')) +
  geom_polygon(data=fortify(hconserv)%>% filter(lat>600000),aes(x=long,y=lat,group=group,fill = 'conserv')) +
  geom_path(data=fortify(hgrid)%>% filter(lat>600000),aes(x=long,y=lat,group=group),col = 'black') +
  theme_map() + theme(legend.position = c(0.05,0.2),legend.text = element_text(size=14),
                      legend.background = element_rect(fill=alpha('white', 0.4))) +
  scale_fill_colorblind(name = '',labels=c('CDD','Incorporated place','Conservation area')) +  coord_sf(datum=NA)

ggsave(figure_3,dpi = 300,filename = 'figure_3.tiff',device = 'tiff',units = 'in',height=6,width=6)

gg <- ggplot() +   geom_path(data = fortify(hills_sp) %>% filter(lat>600000),aes(x=long,y=lat,group=group)) + 
  theme_map()


figure_3
hills_df = fortify(hills)
hills_hex_df = fortify(hex_grid[hills,])


hex_df_2010 = hex_df %>% rename(id = hex_id) %>% ungroup() %>%
  filter(year == 2010) %>% mutate(id = as.character(id))
sf_grid_2010 = left_join(sf_grid %>% mutate(id = as.character(id)),hex_df_2010)
sf_hills_grid_2010 = sf_grid_2010[st_intersects(sf_grid_2010,hills,sparse = F),]


gg2A <- gg + geom_sf(data = sf_hills_grid_2010,aes(fill = 100*dev_area_intensity_last), alpha = 0.7,colour = 'grey80')  + scale_fill_viridis(name = '% developed',option = 'A') +
                ggtitle('Prior devel. intensity (2006)')+
  theme(title = element_text(size = 14),
        legend.background = element_rect(fill=alpha('white', 0.4)),legend.text=element_text(size=12))  +  coord_sf(datum=NA) + NULL 

gg2B <- gg + geom_sf(data = sf_hills_grid_2010,aes(fill = dev_area_intensity_change), alpha = 0.7,colour = 'grey80')  + 
  scale_fill_viridis(name =expression(Delta~development),option = 'D') + 
  ggtitle(expression(paste(Delta," devel. (2006-2010)", sep="")))+
  theme(title = element_text(size = 14),
        legend.background = element_rect(fill=alpha('white', 0.4)),legend.text=element_text(size=12))  +  coord_sf(datum=NA) + NULL

gg2C <- gg + geom_sf(data = sf_hills_grid_2010,
                     aes(fill = intensity_reversed_sprawl_index),
                     alpha = 0.7,colour = 'grey80')  + scale_fill_viridis(name = 'Sprawl intensity',option = 'A')  + 
  ggtitle('Prior sprawl intensity (2006)')+
  theme(title = element_text(size = 14), 
        legend.background = element_rect(fill=alpha('white', 0.4)),legend.text=element_text(size=12))  +  coord_sf(datum=NA) + NULL

gg2D <- gg + geom_sf(data = sf_hills_grid_2010,
                     aes(fill = intensity_reversed_sprawl_index_change),
                     alpha = 0.7,colour = 'grey80')  + scale_fill_viridis(name =expression(Delta~sprawl),option = 'D') +
  ggtitle(expression(paste(Delta," sprawl (2006-2010)", sep="")))+
  theme(title = element_text(size = 14),
        legend.background = element_rect(fill=alpha('white', 0.4)),legend.text=element_text(size=12)) + coord_sf(datum=NA) + NULL

library(gridExtra)

figure_2 <- grid.arrange(gg2A,gg2B,gg2C,gg2D,ncol=2)
figure_2
ggsave(figure_2,dpi = 300,filename = 'figure_2.tiff',device = 'tiff',units = 'in',height=6,width=7)

