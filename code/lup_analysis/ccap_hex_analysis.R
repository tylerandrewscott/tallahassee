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
keep_county = c('Miami-Dade','Manatee','Hillsborough','Leon',
  'St Johns','Bay','Duval',
  'Orange','St Lucie','Polk',
  'Nassau','Osceola','Flagler','Lee')

fl = readOGR('spatial_input/government_units/','county_nrcs_a_fl')
fl_ccaps <- lapply(grep('fl_[0-9]{4}_ccap',list.files('spatial_input/'),value=T),function(x) raster(paste0('spatial_input/',x)))
fl_ccap_stack <- do.call(stack,fl_ccaps)
fl = spTransform(fl,CRS(proj4string(fl_ccap_stack)))
fl@data$COUNTYNAME = gsub('\\.','',fl@data$COUNTYNAME)
fl_sub <- fl[fl@data$COUNTYNAME %in% keep_county,]
size <- 4000

# read_csv('input/census/PEP_2016_PEPANNRES_with_ann.csv') %>% filter(grepl('[0-9]{5}',GEO.id2)) %>%
#   filter(grepl('^12',GEO.id2)) %>% dplyr::select(GEO.id2,respop72016) %>% 
#   rename(FIPS = GEO.id2) %>%
#   mutate(in_sample = (FIPS %in% county.fips[,1][county.fips[,2] %in% 
#                     paste('florida',tolower(keep_county),sep=',')]) +0) %>%
#   group_by(in_sample) %>% summarise(tot_pop = sum(as.numeric(respop72016))) 
# 
#   

hex_points <- spsample(fl_sub, type = "hexagonal", cellsize = size,offset=c(0,0))
hex_grid <- HexPoints2SpatialPolygons(hex_points, dx = size)
rasterOptions(maxmemory = 1e+09)
rmask <- matrix(c(1,1,1,1,1,
                  1,1,1,1,1,
                  1,1,0,1,1,
                  1,1,1,1,1,
                  1,1,1,1,1),nrow=5)
library(rasterVis)

source('code/sprawlIndex.R')   
source('code/devArea.R')

hex_rasters <- pblapply(1:length(hex_grid),function(x) trim(mask(crop(fl_ccap_stack,hex_grid[x,]),hex_grid[x,])),cl=24)
#1:length(hex_rasters)

hex_generalized_sprawl_index <- pblapply(1:length(hex_grid),function(x) sapply(names(hex_rasters[[x]]), function(r) sprawl_index_function(hex_rasters[[x]][[r]],mask = rmask,functionType = 'generalized')),cl = 24)
hex_inverted_sprawl_index <- pblapply(1:length(hex_grid),function(x) sapply(names(hex_rasters[[x]]), function(r) sprawl_index_function(hex_rasters[[x]][[r]],mask = rmask,functionType = 'inverted')),cl = 24)
hex_intensity_sprawl_index <- pblapply(1:length(hex_grid),function(x) sapply(names(hex_rasters[[x]]), function(r) sprawl_index_function(hex_rasters[[x]][[r]],mask = rmask,functionType = 'intensity')),cl = 24)

hex_generalized_sprawl_df <- do.call(rbind,hex_generalized_sprawl_index) %>% as.data.frame() %>% mutate(hex_id = 1:nrow(.)) %>% gather(year,value,-hex_id) %>% mutate(var = 'generalized_sprawl_index')
hex_inverted_sprawl_df <- do.call(rbind,hex_inverted_sprawl_index) %>% as.data.frame() %>% mutate(hex_id = 1:nrow(.)) %>% gather(year,value,-hex_id) %>% mutate(var = 'inverted_sprawl_index')
hex_intensity_sprawl_df <- do.call(rbind,hex_intensity_sprawl_index) %>% as.data.frame() %>% mutate(hex_id = 1:nrow(.)) %>% gather(year,value,-hex_id) %>% mutate(var = 'intensity_sprawl_index')

hex_sprawl_df = Reduce(full_join,lapply(grep('^hex_.*sprawl_df$',ls(),value=T),get))

source('code/devArea.R')
dev_area_generalized <- pblapply(hex_rasters,function(x) sapply(names(x), function(r) dev_function(x[[r]],functionType =  'generalized')),cl = 24)
dev_area_low_intensity <- pblapply(hex_rasters,function(x) sapply(names(x), function(r) dev_function(x[[r]],functionType =  'low_intensity')),cl = 24)
dev_area_intensity <- pblapply(hex_rasters,function(x) sapply(names(x), function(r) dev_function(x[[r]],functionType =  'intensity')),cl = 24)

dev_area_generalized_df <- do.call(rbind,dev_area_generalized) %>% as.data.frame() %>% mutate(hex_id = 1:nrow(.)) %>% gather(year,value,-hex_id) %>% mutate(var = 'dev_area_generalized')
dev_area_intensity_df <- do.call(rbind,dev_area_intensity) %>% as.data.frame() %>% mutate(hex_id = 1:nrow(.)) %>% gather(year,value,-hex_id) %>% mutate(var = 'dev_area_intensity')
dev_area_low_intensity_df <- do.call(rbind,dev_area_low_intensity) %>% as.data.frame() %>% mutate(hex_id = 1:nrow(.)) %>% gather(year,value,-hex_id) %>% mutate(var = 'dev_area_low_intensity')
dev_area_df = Reduce(f = dplyr::full_join,lapply(grep('dev_area_.*_df$',ls(),value=T),get))  

hex_df = full_join(hex_sprawl_df,dev_area_df) %>% 
  mutate(year = str_extract(year,'[0-9]{4}')) %>% spread(var,value)

fl_places <- readOGR('spatial_input/government_units','tl_2016_12_place')
fl_places <- fl_places[!grepl('CDP',fl_places@data$NAMELSAD),]
fl_places = spTransform(fl_places,CRS(proj4string(fl_ccap_stack)))
fl_places = fl_places[fl_sub,]

cdds <- readOGR('spatial_input','master_cdd_dataset')
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
over_cdd_zone = pblapply(1:nrow(hex_df),function(x) gIntersection(hex_grid[hex_df$hex_id[x],],cdds[cdds@data$Year_Created<=hex_df$year[x],]),cl = 24)
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
over_conserv_zone = pblapply(1:length(hex_grid),function(x) gIntersection(hex_grid[x,],conserv_fl_state_fed),cl = 10)
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

dvs <- grep('intensity_reversed_sprawl_index_change|dev_area_intensity_change',names(hex_df),value=T)
save.image('scratch/premod_results.RData')

load('scratch/premod_results.RData')

base_forms <- list("~ 1 + dev_area_intensity_last + 
  prop_over_place + prop_over_conserv + 
  period_standardized_pcsqft_change +  f(county,model = 'iid') + f(year,model = 'iid') +
  f(idx, model = 'besag', graph = hex_adj,scale.model = TRUE)",
                   "~ 1 + intensity_sprawl_index_last + dev_area_intensity_last + 
  prop_over_place + prop_over_conserv + 
  period_standardized_pcsqft_change + f(county,model = 'iid') + f(year,model = 'iid') +
  f(idx, model = 'besag', graph = hex_adj,scale.model = TRUE)")
names(base_forms) <- dvs

base_mods <- lapply(dvs,function(x) 
  inla(as.formula(paste0(x,base_forms[x])), data = hex_df[hex_df$year!=1996,], 
       control.predictor = list(compute = TRUE),control.compute = list(cpo=TRUE,dic=TRUE,waic=TRUE)))

full_forms <- list("~ 1 + dev_area_intensity_last + 
  prop_over_place + prop_over_conserv + 
  period_standardized_pcsqft_change + prop_over_cdd + f(county,model = 'iid') + f(year,model = 'iid') +
  f(idx, model = 'besag', graph = hex_adj,scale.model = TRUE)",
"~ 1 + intensity_sprawl_index_last + dev_area_intensity_last + 
  prop_over_place + prop_over_conserv + 
  period_standardized_pcsqft_change + prop_over_cdd + 
f(county,model = 'iid') + f(year,model = 'iid') +
  f(idx, model = 'besag', graph = hex_adj,scale.model = TRUE)")
names(full_forms) <- dvs
full_mods <- lapply(dvs,function(x) 
  inla(as.formula(paste0(x,full_forms[x])), data = hex_df[hex_df$year!=1996,], 
       control.predictor = list(compute = TRUE),control.compute = list(cpo=TRUE,dic=TRUE,waic=TRUE))
)


base_results = Reduce(full_join,lapply(1:length(base_mods),function(x) base_mods[[x]]$summary.fixed[,c(1,3,5)] %>% cbind(.,dev = dvs[x]) %>%
                                         mutate(coef = rownames(.),mod = 'base'))) 


full_results = Reduce(full_join,lapply(1:length(full_mods),function(x) full_mods[[x]]$summary.fixed[,c(1,3,5)] %>% cbind(.,dev = dvs[x]) %>%
                          mutate(coef = rownames(.),mod = 'full')))

res_table = full_join(base_results,full_results) %>% dplyr::select(-mean) %>% 
  mutate(CI = paste(sprintf("%.3f",round(`0.025quant`,3)),sprintf("%.3f",round(`0.975quant`,3)),sep=', ')) %>%
  mutate(dev_mod = paste(dev,mod)) %>%
  dplyr::select(-dev,-mod,-`0.025quant`,-`0.975quant`) %>% spread(dev_mod,CI)

save.image('scratch/mod_results.RData')


rt = full_join(base_results,full_results) %>% dplyr::select(-mean) %>% 
  mutate(CI = paste(sprintf("%.3f",round(`0.025quant`,3)),sprintf("%.3f",round(`0.975quant`,3)),sep=', ')) %>%
  mutate(dev_mod = paste(dev,mod)) %>%
  dplyr::select(-dev,-mod,-`0.025quant`,-`0.975quant`) %>%
  rbind(.,data.frame(coef = c(rep('dic',4),rep('waic',4)),
                         CI = c(round(sapply(base_mods,function(x) x$dic$dic),3),
                           round(sapply(full_mods,function(x) x$dic$dic),3),
                           round(sapply(base_mods,function(x) x$waic$waic),3),
                           round(sapply(full_mods,function(x) x$waic$waic),3)),
                         dev_mod = c('dev_area_intensity_change base',
                                     'intensity_reversed_sprawl_index_change base',
                                     'dev_area_intensity_change full',
                                     'intensity_reversed_sprawl_index_change full'))) %>% spread(dev_mod,CI)

library(stargazer)
stargazer(rt[c(1,2,4,5,7,8,6,3,9),],summary=F,out = 'scratch/result_table.html')

gArea(fl_sub)/gArea(fl)

summary(hex_df$dev_area_intensity_change)
summary(hex_df$intensity_reversed_sprawl_index_change)


stargazer::stargazer(base_results %>% dplyr::select(-mean) %>% 
                       mutate(CI = paste(sprintf("%.3f",round(`0.025quant`,3)),sprintf("%.3f",round(`0.975quant`,3)),sep=', ')) %>%
                       dplyr::select(-mod,-`0.025quant`,-`0.975quant`) %>% spread(dev,CI),summary = F,out = 'scratch/base_result_table.html')

full_join(full_results %>% dplyr::select(-mean) %>% 
                       mutate(CI = paste(sprintf("%.3f",round(`0.025quant`,3)),sprintf("%.3f",round(`0.975quant`,3)),sep=', ')) %>%
                       dplyr::select(-`0.025quant`,-`0.975quant`) %>% spread(dev,CI),
base_results %>% dplyr::select(-mean) %>%
                       mutate(CI = paste(sprintf("%.3f",round(`0.025quant`,3)),sprintf("%.3f",round(`0.975quant`,3)),sep=', ')) %>%
                       dplyr::select(-`0.025quant`,-`0.975quant`) %>% spread(dev,CI)) 



std_vars = c('prop_over_cdd','prop_over_conserv','prop_over_place','dev_area_intensity_last',
             'dev_area_intensity_change','intensity_reversed_sprawl_index_change',
             'intensity_reversed_sprawl_index_last')

results = full_join(base_results,full_results)

rs = results %>% dplyr::select(-mean)

cbind(coef = rs[rs$dev=='dev_area_intensity_change'&rs$mod=='base',4],
      round(rs[rs$dev=='dev_area_intensity_change'&rs$mod=='base',1:2],4))
cbind(coef = rs[rs$dev=='dev_area_intensity_change'&rs$mod=='full',4],
      round(rs[rs$dev=='dev_area_intensity_change'&rs$mod=='full',1:2],4))

cbind(coef = rs[rs$dev=='intensity_reversed_sprawl_index_change'&rs$mod=='base',4],
      round(rs[rs$dev=='intensity_reversed_sprawl_index_change'&rs$mod=='base',1:2],3))
cbind(coef = rs[rs$dev=='intensity_reversed_sprawl_index_change'&rs$mod=='full',4],
      round(rs[rs$dev=='intensity_reversed_sprawl_index_change'&rs$mod=='full',1:2],3))


# 
# coef_tab = full_join(base_mod$summary.fixed[,c(1,3,5)]  %>% mutate(coef = as.factor(rownames(.)),model = 'Restricted model'),
#                      full_mod$summary.fixed[,c(1,3,5)] %>% mutate(coef = as.factor(rownames(.)),model = 'CDD overlap model'))
# library(forcats)
# 
# t1  = base_mod$summary.fixed[,c(3,5)]  %>% mutate(coef = rownames(.),model = 'Restricted model',`0.025quant` = round(`0.025quant`,3),`0.975quant` = round(`0.975quant`,3))
# t1 = rbind(t1,c(NA,NA,'prop_over_cdd','Restricted model'))
# t2 = full_mod$summary.fixed[,c(3,5)] %>% mutate(coef = rownames(.),model = 'CDD overlap model',`0.025quant` = round(`0.025quant`,3),`0.975quant` = round(`0.975quant`,3))
# ctab = data.frame(cbind(t1[,c(3,1,2)],t2[,c(1,2)]))
# 
# library(stargazer)
# stargazer(ctab,summary = FALSE,out = 'scratch/robust_table.html')
# 
# cbind(NA,NA,c('WAIC','DIC'),round(do.call(rbind,lapply(list(base_mod,full_mod),function(x)c(x$waic$waic,x$dic$dic))),3))
# 
# ggplot(coef_tab) + geom_errorbarh(aes(xmin = `0.025quant`,xmax = `0.975quant`,y = coef,x =mean, group = model,colour = model),position = 'dodge')
# 
# 



#####plotting examples######

#make sample plot
hgrid = hex_grid[fl_sub[fl_sub$COUNTYNAME=='Hillsborough',],]
hcounty = fl_sub[fl_sub$COUNTYNAME=='Hillsborough',]
hplaces = gIntersection(fl_places,hcounty)
hconserv = gIntersection(conserv_fl_state_fed,hcounty)
hcdd = gIntersection(cdds,hcounty)

figure_4 <- ggplot() +
  geom_polygon(data=fortify(hcounty) %>% filter(lat>600000),aes(x=long,y=lat,group=group),fill = 'grey95',col = 'grey60') +
  geom_polygon(data=fortify(hplaces)%>% filter(lat>600000),aes(x=long,y=lat,group=group,fill = 'city')) +
  geom_polygon(data=fortify(hcdd)%>% filter(lat>600000),aes(x=long,y=lat,group=group,fill = 'cdd')) +
  geom_polygon(data=fortify(hconserv)%>% filter(lat>600000),aes(x=long,y=lat,group=group,fill = 'conserv')) +
  geom_path(data=fortify(hgrid)%>% filter(lat>600000),aes(x=long,y=lat,group=group),col = 'black') +
  theme_map() + theme(legend.position = c(0.05,0.2),legend.text = element_text(size=14),
                      legend.background = element_rect(fill=alpha('white', 0.4))) +
  scale_fill_colorblind(name = '',labels=c('CDD','Incorporated place','State/federal conservation'))

hills_df = fortify(fl_sub[fl_sub$COUNTYNAME=='Hillsborough',])
hills_hex_df = fortify(hex_grid[fl_sub[fl_sub$COUNTYNAME=='Hillsborough',],],region = 'ID')

summary(hex_df$dev_area_intensity_change)
summary(hex_df$generalized_sprawl_index_change)
summary(hex_df$prop_over_cdd)
plot(hex_df$generalized_sprawl_index_last ~ hex_df$dev_area_intensity_last)
gg <- ggplot() + geom_path(data = hills_df %>% filter(lat>600000),aes(x = long, y = lat,group = group)) +
 theme_map()

hills_hex_df_2010 = left_join(hills_hex_df,hex_df %>% rename(id = hex_id) %>% group_by() %>%
            filter(year == 2010) %>% mutate(id = as.character(id)))

gg3A <- gg + geom_polygon(data = hills_hex_df_2010,
               aes(x = long, y = lat,group = group,fill = 100*dev_area_intensity_last),
                 alpha = 0.7,colour = 'grey80')  + scale_fill_viridis(name = '% developed',option = 'A') + 
                ggtitle('Prior development intensity (2006)')+
  theme(legend.position = c(0.0,0.1),title = element_text(size = 14),
        legend.background = element_rect(fill=alpha('white', 0.4)),legend.text=element_text(size=12)) 
gg3B <- gg + geom_polygon(data = hills_hex_df_2010,
                  aes(x = long, y = lat,group = group,fill = dev_area_intensity_change),
                  alpha = 0.7,colour = 'grey80')  + scale_fill_viridis(name =expression(Delta~development),option = 'D') + 
                  ggtitle(expression(paste(Delta," development (2006-2010)", sep="")))+
  theme(legend.position = c(0.0,0.1),title = element_text(size = 14),
        legend.background = element_rect(fill=alpha('white', 0.4)),legend.text=element_text(size=12)) 
gg3C <- gg + geom_polygon(data = hills_hex_df_2010,
                  aes(x = long, y = lat,group = group,fill = intensity_reversed_sprawl_index),
                  alpha = 0.7,colour = 'grey80')  + scale_fill_viridis(name = 'Sprawl intensity',option = 'A')  + 
                  ggtitle('Prior sprawl intensity (2006)')+
  theme(legend.position = c(0.0,0.1),title = element_text(size = 14),
        legend.background = element_rect(fill=alpha('white', 0.4)),legend.text=element_text(size=12)) 
gg3D <- gg + geom_polygon(data = hills_hex_df_2010,
                  aes(x = long, y = lat,group = group,fill = intensity_reversed_sprawl_index_change),
                  alpha = 0.7,colour = 'grey80')  + scale_fill_viridis(name =expression(Delta~sprawl),option = 'D') +
            ggtitle(expression(paste(Delta," sprawl (2006-2010)", sep="")))+
  theme(legend.position = c(0.0,0.1),title = element_text(size = 14),
        legend.background = element_rect(fill=alpha('white', 0.4)),legend.text=element_text(size=12)) 
  
library(gridExtra)
ggplot() + ggtitle(expression(atop(paste(Delta," development", sep=""))))


figure_3 <- grid.arrange(gg3A,gg3B,gg3C,gg3D,ncol=2)



hex_df$intensity_reversed_sprawl_index
hex_df$dev_area_intensity_change
hex_df  %>% spread(year,dev_area_intensity_change)
df_spdf_change = hex_df %>% select(-prior_dev,-dev_outcome) %>% spread(period,dev_change)
df_spdf_start = hex_df %>% select(-dev_change,-dev_outcome) %>% spread(period,prior_dev) %>%
  rename(`1996` = `1996-2001`,`2001` = `2001-2006`,`2006` = `2006-2010`)
df_spdf = full_join(df_spdf_change,df_spdf_start)
hgrid_spdf = SpatialPolygonsDataFrame(hex_grid,data = df_spdf)

hills_df = fortify(hgrid_spdf[fl_sub[fl_sub$COUNTYNAME=='Hillsborough',],],region = 'ID')
hills_df = hills_df %>% rename(ID = id) %>% mutate(ID = as.numeric(ID))
hills_df = left_join(hills_df,hgrid_spdf@data)

hill_start_df = hills_df %>% select(-`1996-2001`,-`2001-2006`,-`2006-2010`) %>%
  gather(key = Year,value = Dev,
         -county,-prop_over_cdd,-prop_over_place,-group,-piece,-hole,-order,-lat,-long,-ID,-idx)
hill_change_df = hills_df %>% select(-`1996`,-`2001`,-`2006`) %>%
  gather(key = Period,value = Change,
         -county,-prop_over_cdd,-prop_over_place,-group,-piece,-hole,-order,-lat,-long,-ID,-idx)

hcounty_df = fortify(hcounty)
hplaces_df = fortify(hplaces)
hcdd_df = fortify(hcdd)

gg1 = ggplot() + geom_polygon(data = hill_start_df %>% filter(Year == 1996),
                              aes(y = lat,x = long,group = group,fill = Dev),
                              alpha = 0.7,colour = 'grey80') +
  scale_fill_viridis(name = 'developed (%)',option = 'D',limits=c(0,100)) + theme_map()  +
  geom_path(aes(y = lat,x = long,group = group),data = hcounty_df %>% filter(lat>600000)) +
  theme(legend.position = c(0.0,0.1),title = element_text(size = 14),
        legend.background = element_rect(fill=alpha('white', 0.4)),legend.text=element_text(size=12)) + ggtitle('Developed land % in 1996')

gg2 = ggplot() + geom_polygon(data = hill_change_df %>% filter(Period == '1996-2001'),
                              aes(y = lat,x = long,group = group,fill = Change),
                              alpha = 0.7,colour = 'grey80') +
  scale_fill_viridis(name = 'change (%)',option = 'A',limits=c(-1,20),breaks=seq(0,25,5)) + theme_map()  +
  geom_path(aes(y = lat,x = long,group = group),data = hcounty_df %>% filter(lat>600000)) +
  theme(legend.position = c(0.0,0.1),title = element_text(size = 14),
        legend.background = element_rect(fill=alpha('white', 0.4)),legend.text=element_text(size=12)) +
  ggtitle('Change in % developed 1996-2001')

gg3 = ggplot() + geom_polygon(data = hill_change_df %>% filter(Period == '2001-2006'),
                              aes(y = lat,x = long,group = group,fill = Change),
                              alpha = 0.7,colour = 'grey80') +
  scale_fill_viridis(name = 'change (%)',option = 'A',limits=c(-1,20),breaks=seq(0,25,5)) + theme_map()  +
  geom_path(aes(y = lat,x = long,group = group),data = hcounty_df %>% filter(lat>600000)) +
  theme(legend.position = c(0.0,0.1),title = element_text(size = 14),
        legend.background = element_rect(fill=alpha('white', 0.4)),legend.text=element_text(size=12)) +
  ggtitle('Change in % developed 2001-2006')

gg4 = ggplot() + geom_polygon(data = hill_change_df %>% filter(Period == '2006-2010'),
                              aes(y = lat,x = long,group = group,fill = Change),
                              alpha = 0.7,colour = 'grey80') +
  scale_fill_viridis(name = 'change (%)',option = 'A',limits=c(-1,20),breaks=seq(0,25,5)) + theme_map()  +
  geom_path(aes(y = lat,x = long,group = group),data = hcounty_df %>% filter(lat>600000)) +
  theme(legend.position = c(0.0,0.1),title = element_text(size = 14),
        legend.background = element_rect(fill=alpha('white', 0.4)),legend.text=element_text(size=12)) +
  ggtitle('Change in % developed 2006-2010')



library(gridExtra)
grid.arrange(gg1,gg2,gg3,gg4)

base_mod$waic$waic
full_mod$waic$waic
base_mod$dic$dic
full_mod$dic$dic
summary(full_mod)
library(INLA)
form = Value ~ 1 + f(county,model = 'iid') + f(Year,model = 'ar1',replicate = ID,fixed=TRUE)
mod = inla(formula = form,data = hex_df,family = 'gaussian',
           control.fixed = list(expand.factor.strategy='inla'),control.predictor = list(compute = TRUE),
           control.inla= list(int.strategy = "eb",strategy = "gaussian",diagonal = 1000),verbose=T)

nm = gsub('(II$|IV$|III$|I$)','Community Development District \\1',cdds@data$CDD_NAME)
nm = stringr::str_to_title(nm)
nm[!grepl('Community Development District|CCD',nm)&!grepl('[0-9]$',nm)] <- paste(nm[!grepl('Community Development District|CCD',nm)&!grepl('[0-9]$',nm)],'Community Development District',sep=' ')
nm = gsub('Pineyz','Piney-Z',nm)
nm = gsub('Southfork','South Fork',nm)
nm <- gsub(' ([0-9]$)',' Community Development District \\1',nm)

cdd_admin = read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSjykKij_RQ6uT6K3zIuEc0PxWhSEcS-PifLWOUmX7ryyEKIrgF0BKNiK8pFoL7h8nal9koPkb36UTc/pub?output=csv') %>%
  filter(Function=='Community Development')
table(lubridate::year(lubridate::dmy(cdd_admin$`Date Created / Established`)))
table(cdd_admin$`Date Created / Established`)
dim(cdd_admin)



head(test@data)
table(test@data$TYPE)
dim(cdds)
table(!duplicated(cdds$CDD_NAME))
test@data$NAME[test@data$TYPE=='PB']

test@data[grep('ASTURIA',test@data$NAME),]

grep('Golden',test$NameNum,value=T)
head(test@data)
lubd = dmy(cdd_admin$`Date Created / Established`)

library(fragstats)
library(landscapeR)
# All_CLUMPY  <- do.call(rbind,pblapply(1:nrow(hex_df),function(i) {
#   hcrop_all <- crop(fl_ccaps_stack_all_dev[[paste0('Developed.',hex_df$Year[i])]],hex_grid[hex_df$ID[i],]);
#   hmask_all <- mask(hcrop_all,hex_grid[hex_df$ID[i],]);
#   cs_alldev <- ClassStat(hmask_all,cellsize = 1);
#   data.frame(row = i, ID = hex_df$ID[i],clumpy_1 = compute_clumpy(cs_alldev))
#   },cl = 20))
?mask
length(hex_rasters[[5]][[1]])
freq(hex_rasters[[5]][[1]])
30 + 64 + 39 + 7 + 4819 + 4424 + 1749 + 4274 + 41 

compute_clumpy = function(df) {
   if(all(df$class!=1)){return(1)}
   else if(df$prop.landscape[df$class==1]==0){return(1)}
   else if(df$prop.like.adjacencies[df$class==1]==0 & 
           df$prop.landscape[df$class==1]==1){return(-1)}
   else if(df$prop.like.adjacencies[df$class==1] < df$prop.landscape[df$class==1] & df$prop.landscape[df$class==1] < 0.5)
   {return((df$prop.like.adjacencies[df$class==1]-df$prop.landscape[df$class==1]) / 
             (df$prop.landscape[df$class==1]))}
   else {return((df$prop.like.adjacencies[df$class==1]-df$prop.landscape[df$class==1])/
                  (1-df$prop.landscape[df$class==1]))}}


ggplot() + geom_histogram(aes(x=lubd)) + scale_x_date(name = 'Date') +
  scale_y_continuous(name = '# CDDs formed') +
  geom_vline(xintercept=mdy('6/21/1996'),lty=2,col='black') +
  geom_vline(xintercept=mdy('6/21/2001'),lty=2,col='black') +
  geom_vline(xintercept=mdy('6/21/2006'),lty=2,col='black') +
  geom_vline(xintercept=mdy('6/21/2010'),lty=2,col='black') + theme_bw()


dim(cdd_admin)
library(stringr)
cdd_admin$`District's Name` = stringr::str_to_title(cdd_admin$`District's Name`)
cdd_admin$`District's Name` = gsub('\\.','',cdd_admin$`District's Name`)
cdd_admin$`District's Name`[cdd_admin$`District's Name`==  "Fishhawk Community Development District"] <-  "Fishhawk Community Development District I"
dim(cdd_admin)
#

# 
# 
# si_dev <- mclapply(fl,function(r) sprawl_index_function(raster = mask(crop(fl_ccap_stack,hex_grid[100,]),hex_grid[100,]),mask = rmask, functionType = 'developed'),mc.cores = 24,mc.cleanup = T,mc.preschedule = T)
# si_dev_weighted <-mclapply(fl,function(r) sprawl_index_function(raster = raster(paste0(pref,r)),mask = rmask,functionType =  'weighted'),mc.cores = 24,mc.cleanup = T,mc.preschedule = T)
# source('code/devArea.R')
# dev_area_weighted <-mclapply(fl,function(r) dev_function(raster = raster(paste0(pref,r)),functionType =  'weighted'),mc.cores = 24,mc.cleanup = T,mc.preschedule = T)
# dev_area <-mclapply(fl,function(r) dev_function(raster = raster(paste0(pref,r)),functionType =  'developed'),mc.cores = 24,mc.cleanup = T,mc.preschedule = T)
# 
# temp = data.frame(Year = as.numeric(as.character(str_extract(fl,'(?!fl_)[0-9]{4}'))), 
#                   FIPS = str_extract(fl,'[0-9]{5}'),
#                   developed_area = unlist(dev_area),
#                   developed_area_weighted = unlist(dev_area_weighted),
#                   sprawl_developement = unlist(si_dev),
#                   sprawl_development_weighted = unlist(si_dev_weighted))
# counties = readOGR('spatial_input/government_units/','tl_2016_us_county')
# temp$Total_Area_SqMiles <- {as.numeric(as.character(counties@data$ALAND[match(temp$FIPS,paste0(counties@data$STATEFP,counties@data$COUNTYFP))])) +
#     as.numeric(as.character(counties@data$AWATER[match(temp$FIPS,paste0(counties@data$STATEFP,counties@data$COUNTYFP))]))} * 0.00000038610
# temp$Land_Area_SqMiles <- {as.numeric(as.character(counties@data$ALAND[match(temp$FIPS,paste0(counties@data$STATEFP,counties@data$COUNTYFP))]))} * 0.00000038610
# 
# 
# #0-20,21-49,50-79,80-100
# p1 = read_csv('input/census/PEP_2016_PEPANNRES_with_ann.csv',skip=1) %>% dplyr::select(-Id,-Geography) %>%
#   gather(Year,Est,-Id2) %>% mutate(Year = str_extract(Year,'[0-9]{4}')) %>% rename(FIPS = Id2) %>% filter(!is.na(FIPS))
# p2 = read_csv('input/census/co-est00int-tot.csv',skip=0) %>% mutate(STATE = sprintf("%02i",STATE),
#                                                                     COUNTY = sprintf("%03i",COUNTY),
#                                                                     FIPS = paste0(STATE,COUNTY)) %>%
#   filter(COUNTY!='000') %>%
#   dplyr::select(-SUMLEV,-REGION,-DIVISION,-STNAME,-CTYNAME,-STATE,-COUNTY,-ESTIMATESBASE2000 ) %>%
#   gather(Year,Est,-FIPS) %>% mutate(Year = str_extract(Year,'[0-9]{4}'))
# p3 = read_table('input/census/stch-icen1996.txt',col_names = F) %>% rename(Year = X1,FIPS = X2,Pop = X6) %>% 
#   mutate(Year = paste0('19',Year)) %>%
#   group_by(Year,FIPS) %>% summarise(Est = sum(Pop))
# pops = Reduce(full_join,list(p1,p2,p3)) %>% mutate(Year = as.numeric(Year))
# 
# temp <- left_join(temp,pops)
# 
# #Year
# #FIPS code state
# #FIPS code county
# #Age Group
# #Race-Sex
# #Ethnic origin
# #POP
# 
# 
# temp <- temp %>% filter(grepl('^12',FIPS))
# 
# 
# 
# 
# cat_dev_class_matrix = rbind(cbind(c(0:1),NA),cbind(c(2:5),2:5),cbind(c(6:25),0))
# all_dev_class_matrix = rbind(cbind(c(0:1),NA),cbind(c(2:5),1),cbind(c(6:25),0))
# low_dev_class_matrix = rbind(cbind(c(0:1),NA),cbind(c(2:3),0),cbind(c(4),1),cbind(c(5:25),0))
# 
# #fl_ccaps_stack_all_dev <- raster::reclassify(x = fl_ccap_stack,rcl = all_dev_class_matrix,right = NA)
# #fl_ccaps_stack_low_dev <- raster::reclassify(x = fl_ccap_stack,rcl = low_dev_class_matrix,right = NA)
# #names(fl_ccaps_stack_all_dev) <- c("Developed 1996","Developed 2001","Developed 2006","Developed 2010")
# #names(fl_ccaps_stack_low_dev) <- c("Low Developed 1996","Low Developed 2001","Low Developed 2006","Low Developed 2010")
# #writeRaster(fl_ccaps_stack_all_dev, filename="spatial_input/FL_Dev_CCAPS/fl_ccaps_stack_dev.grd",  overwrite=TRUE,format = 'raster',prj=TRUE)
# #writeRaster(fl_ccaps_stack_low_dev, filename="spatial_input/FL_Dev_CCAPS/fl_ccaps_stack_low_dev.grd", overwrite=TRUE,format = 'raster',prj = TRUE)
# fl_ccaps_stack_all_dev <- 
#   sapply(1:4,function(b) raster('spatial_input/FL_Dev_CCAPS/fl_ccaps_stack_dev.grd',band=b))
# fl_ccaps_stack_all_dev <- stack(fl_ccaps_stack_all_dev)
# 
# fl_ccaps_stack_low_dev <- 
#   sapply(1:4,function(b) raster('spatial_input/FL_Dev_CCAPS/fl_ccaps_stack_low_dev.grd',band=b))
# fl_ccaps_stack_low_dev <- stack(fl_ccaps_stack_low_dev)
# 
# hex_averages_dev = raster::extract(fl_ccaps_stack_all_dev,hex_grid,method = 'simple',fun = mean,na.rm=TRUE,df = TRUE)
# hex_averages_low_dev = raster::extract(fl_ccaps_stack_low_dev,hex_grid,method = 'simple',fun = mean,na.rm=TRUE,df = TRUE)
# 
# library(diseasemapping)
# library(tidyr)
# hex_dev_df = hex_averages_dev %>% gather(Year,Value,-ID) %>% dplyr::mutate(Year = gsub('Developed\\.','',Year)) %>% rename(All_Dev = Value)
# hex_low_dev_df = hex_averages_low_dev %>% gather(Year,Value,-ID) %>% mutate(Year = gsub('Low\\.Developed\\.','',Year)) %>% rename(Low_Dev = Value)
# hex_df <- dplyr::full_join(hex_dev_df,hex_low_dev_df)
# which_county = over(hex_grid,fl_sub)
# hex_df$county = which_county$COUNTYNAME[hex_df$ID]
# library(fragstats)
# library(landscapeR)
# 
# compute_clumpy = function(df) {
#   if(all(df$class!=1)){return(1)}
#   else if(df$prop.landscape[df$class==1]==0){return(1)}
#   else if(df$prop.like.adjacencies[df$class==1]==0 & 
#           df$prop.landscape[df$class==1]==1){return(-1)}
#   else if(df$prop.like.adjacencies[df$class==1] < df$prop.landscape[df$class==1] & df$prop.landscape[df$class==1] < 0.5)
#   {return((df$prop.like.adjacencies[df$class==1]-df$prop.landscape[df$class==1]) / 
#             (df$prop.landscape[df$class==1]))}
#   else {return((df$prop.like.adjacencies[df$class==1]-df$prop.landscape[df$class==1])/
#                  (1-df$prop.landscape[df$class==1]))}}
# 
# #hex_df$All_CLUMPY = hex_df$Low_CLUMPY = NA
# library(parallel)
# library(pbapply)
# 
# 
## library(fragstats)
# library(landscapeR)
# All_CLUMPY  <- do.call(rbind,pblapply(1:nrow(hex_df),function(i) {
#   hcrop_all <- crop(fl_ccaps_stack_all_dev[[paste0('Developed.',hex_df$Year[i])]],hex_grid[hex_df$ID[i],]);
#   hmask_all <- mask(hcrop_all,hex_grid[hex_df$ID[i],]);
#   cs_alldev <- ClassStat(hmask_all,cellsize = 1);
#   data.frame(row = i, ID = hex_df$ID[i],clumpy_1 = compute_clumpy(cs_alldev))
#   },cl = 20))
# 
# All_CLUMPY$clumpy_1
# 
# head(hex_df)
# cbind(hex_df,All_CLUMPY %>% dplyr::select(-ID)) %>% arrange(clumpy_1) %>% mutate(All_Dev = round(All_Dev,2)) %>%
#   filter(clumpy_1 < (-0.3) & clumpy_1 > (-0.4))
# plot(mask(crop(fl_ccaps_stack_all_dev[['Developed.1996']],hex_grid[272,]),hex_grid[272,]))
# 
# 
# cbind(hex_df,All_CLUMPY %>% dplyr::select(-ID)) %>% arrange(clumpy_1) %>% mutate(All_Dev = round(All_Dev,2)) %>%
#   filter(clumpy_1 > (0.3) & clumpy_1 < (0.4))
# plot(mask(crop(fl_ccaps_stack_all_dev[['Developed.1996']],hex_grid[654,]),hex_grid[654,]))
# 
# 
# 
# test <- cbind(hex_df,All_CLUMPY %>% dplyr::select(-ID)) 
# test %>% arrange(clumpy_1) %>% head()
# plot(test$clumpy_1 ~ test$All_Dev)
# 
# 
# hex_grid[hex_df$ID==272&hex_df$Year=='2001',]
# hex_df$ID
# 
# 
# 
# summary(All_CLUMPY$clumpy_1)
# head(All_CLUMPY)
# length(hex_grid)
# 1462 * 4
# summary(All_CLUMPY)
# table(All_CLUMPY$class)
# All_CLUMPY[All_CLUMPY$prop.landscape]
# head(All_CLUMPY)
# i = 
# summary(All_CLUMPY$clumpy_1)
# All_CLUMPY$clumpy_1 <- as.numeric(All_CLUMPY$clumpy_1)
# 
# All_CLUMPY %>% filter(class==1) %>% select(prop.landscape,ID,row,clumpy_1) %>% arrange(clumpy_1) %>% head()
# 
# nrow(hex_df)
# nrow(hex_df)/4
# length(unique(hex_df$ID))
# 
# 
# Low_CLUMPY <- pblapply(1:nrow(hex_df),function(i) {
#   hcrop_low <- crop(fl_ccaps_stack_low_dev[[paste0('Low.Developed.',hex_df$Year[i])]],
#                     hex_grid[hex_df$ID[i],])
#   hmask_low <- mask(hcrop_low,hex_grid[hex_df$ID[i],])
#   cs_lowdev <- ClassStat(hmask_low,cellsize = 1)
#   compute_clumpy(cs_lowdev)},cl = 20)
# save.image('scratch/temp.RData')
# #plot(fl_sub, col = "grey50", bg = "light blue", axes = TRUE)
# #plot(hex_points, col = "black", pch = 20, cex = 0.5, add = T)
# #plot(hex_grid, border = "orange", add = T)
# 
