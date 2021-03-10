

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
rmask <- matrix(c(1,1,1,1,1,
                  1,1,1,1,1,
                  1,1,0,1,1,
                  1,1,1,1,1,
                  1,1,1,1,1),nrow=5)
library(rasterVis)
fl <- list.files('spatial_input/County_CCAPS/',pattern = '^fl.*grd$')
pref <- "spatial_input/County_CCAPS/"
source('code/sprawlIndex.R')               

si_generalized <- pblapply(fl,function(r) sprawl_index_function(raster = raster(paste0(pref,r)),mask = rmask, functionType = 'generalized'),cl = 24)
si_intensity <-pblapply(fl,function(r) sprawl_index_function(raster = raster(paste0(pref,r)),mask = rmask,functionType =  'intensity'),cl = 24)
si_inverse <-pblapply(fl,function(r) sprawl_index_function(raster = raster(paste0(pref,r)),mask = rmask,functionType =  'inverted'),cl = 24)

source('code/devArea.R')
dev_area_generalized <-pblapply(fl,function(r) dev_function(raster = raster(paste0(pref,r)),functionType =  'generalized'),cl = 24)
dev_area_intensity <-pblapply(fl,function(r) dev_function(raster = raster(paste0(pref,r)),functionType =  'intensity'),cl = 24)
dev_area_low_intensity <-pblapply(fl,function(r) dev_function(raster = raster(paste0(pref,r)),functionType =  'low_intensity'),cl = 24)

temp = data.frame(Year = as.numeric(as.character(str_extract(fl,'(?!fl_)[0-9]{4}'))), 
                  FIPS = str_extract(fl,'[0-9]{5}'),
                  developed_area_generalized = unlist(dev_area_generalized),
                  developed_area_intensity = unlist(dev_area_intensity),
                  developed_area_low_intensity = unlist(dev_area_low_intensity),
                  sprawl_index_generalized = unlist(si_generalized),
                  sprawl_index_intensity = unlist(si_intensity),
                  sprawl_index_inverted = unlist(si_inverse))

counties = readOGR('spatial_input/government_units/','tl_2016_us_county')
temp$Total_Area_SqMiles <- {as.numeric(as.character(counties@data$ALAND[match(temp$FIPS,paste0(counties@data$STATEFP,counties@data$COUNTYFP))])) +
  as.numeric(as.character(counties@data$AWATER[match(temp$FIPS,paste0(counties@data$STATEFP,counties@data$COUNTYFP))]))} * 0.00000038610
temp$Land_Area_SqMiles <- {as.numeric(as.character(counties@data$ALAND[match(temp$FIPS,paste0(counties@data$STATEFP,counties@data$COUNTYFP))]))} * 0.00000038610



#0-20,21-49,50-79,80-100
p1 = read_csv('input/census/PEP_2016_PEPANNRES_with_ann.csv',skip=1) %>% dplyr::select(-Id,-Geography) %>% dplyr::select(-`April 1, 2010 - Census`,-`April 1, 2010 - Estimates Base`) %>%
gather(Year,Est,-Id2) %>% mutate(Year = str_extract(Year,'[0-9]{4}')) %>% rename(FIPS = Id2) %>% filter(!is.na(FIPS))
p2 = read_csv('input/census/co-est00int-tot.csv',skip=0) %>% mutate(STATE = sprintf("%02i",STATE),
                                                                    COUNTY = sprintf("%03i",COUNTY),
                                                                    FIPS = paste0(STATE,COUNTY)) %>%
  filter(COUNTY!='000') %>%
  dplyr::select(-SUMLEV,-REGION,-DIVISION,-STNAME,-CTYNAME,-STATE,-COUNTY,-ESTIMATESBASE2000 ) %>%
  gather(Year,Est,-FIPS) %>% mutate(Year = str_extract(Year,'[0-9]{4}')) %>% filter(Year!='2010')
p3 = read_table('input/census/stch-icen1996.txt',col_names = F) %>% rename(Year = X1,FIPS = X2,Pop = X6) %>% 
  mutate(Year = paste0('19',Year)) %>%
  group_by(Year,FIPS) %>% summarise(Est = sum(Pop))
pops = Reduce(full_join,list(p1,p2,p3)) %>% mutate(Year = as.numeric(Year))
pops = pops %>% rename(Population = Est)
temp <- left_join(temp,pops)

#sum(pops$Population[pops$Year==2016 & pops$FIPS %in% fl_sub@data$FIPS_C])/
#sum(pops$Population[pops$Year==2016 & pops$FIPS %in% fl@data$FIPS_C])
temp <- temp %>% filter(grepl('^12',FIPS))

out_cbsa <- tolower(c('Walton','Jackson','Suwannee','Levy','Bradford',
             'Washington','Taylor','Holmes','Madison','Dixie',
             'Gulf','Union','Hamilton','Calhoun','Glades','Franklin',
             'Lafayette','Liberty'))

library(maps)
fl.fips = county.fips %>% filter(grepl('florida',polyname)) %>% rename(COUNTY = polyname,FIPS = fips) %>% mutate(COUNTY = gsub('^florida\\,','',COUNTY))

temp = left_join(temp,fl.fips) %>% rename(Population = Est)

temp$in_cbsa <- (!temp$COUNTY %in% out_cbsa) + 0
temp$sprawl_index_intensity_reversed <- -(temp$sprawl_index_intensity)
#ggplot(temp,aes(y = sprawl_index_intensity_reversed,x = developed_area_intensity,colour = as.factor(in_cbsa))) + geom_point(size=4) +
#  scale_colour_brewer(name = 'In CBSA',labels=c('no','yes'),type = 'qual') + theme_bw()

write.csv(temp,'input/sprawl_dev_indices_version2.csv')
