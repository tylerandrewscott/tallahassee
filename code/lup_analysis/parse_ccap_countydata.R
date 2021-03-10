library(tidyverse)

#prefix = 'input'
prefix = 'Downloads/CCAP_byCounty/'
base96 <- read_csv(paste0(prefix,'LandCover1996.csv')) %>% filter(StateAbbrev == 'FL')

developed_96 <- Reduce(full_join,
                       list(base96 %>% group_by(FIPS,CountyName,Year) %>%
                              summarise(Total_Area_SqMiles = sum(SquareMiles)),
                            base96 %>% 
                              filter(grepl('Intensity Developed',LCName)) %>% group_by(FIPS,CountyName,Year) %>%
                              summarise(Developed_Area_SqMiles = sum(SquareMiles)),
                            base96 %>% 
                              filter(grepl('Intensity Developed',LCName)) %>% group_by(FIPS,CountyName,Year,LCName) %>%
                              summarise(Total_Area_SqMiles = sum(SquareMiles)) %>% spread(LCName,Total_Area_SqMiles) %>%
                              rename(High_Intensity_Developed_SqMiles  = `High Intensity Developed`,
                                     Medium_Intensity_Developed_SqMiles  = `Medium Intensity Developed`,
                                     Low_Intensity_Developed_SqMiles  = `Low Intensity Developed`)))
write_csv(developed_96,'ccap_florida_county_development_1996')

base01 <- read_csv(paste0(prefix,'LandCover2001.csv')) %>% filter(StateAbbrev == 'FL')

developed_01 <- Reduce(full_join,
                       list(base01 %>% group_by(FIPS,CountyName,Year) %>%
                              summarise(Total_Area_SqMiles = sum(SquareMiles)),
                            base01 %>% 
                              filter(grepl('Intensity Developed',LCName)) %>% group_by(FIPS,CountyName,Year) %>%
                              summarise(Developed_Area_SqMiles = sum(SquareMiles)),
                            base01 %>% 
                              filter(grepl('Intensity Developed',LCName)) %>% group_by(FIPS,CountyName,Year,LCName) %>%
                              summarise(Total_Area_SqMiles = sum(SquareMiles)) %>% spread(LCName,Total_Area_SqMiles) %>%
                              rename(High_Intensity_Developed_SqMiles  = `High Intensity Developed`,
                                     Medium_Intensity_Developed_SqMiles  = `Medium Intensity Developed`,
                                     Low_Intensity_Developed_SqMiles  = `Low Intensity Developed`)))
base06 <- read_csv(paste0(prefix,'LandCover2006.csv')) %>% filter(StateAbbrev == 'FL')
developed_06 <- Reduce(full_join,
                       list(base06 %>% group_by(FIPS,CountyName,Year) %>%
                              summarise(Total_Area_SqMiles = sum(SquareMiles)),
                            base06 %>% 
                              filter(grepl('Intensity Developed',LCName)) %>% group_by(FIPS,CountyName,Year) %>%
                              summarise(Developed_Area_SqMiles = sum(SquareMiles)),
                            base06 %>% 
                              filter(grepl('Intensity Developed',LCName)) %>% group_by(FIPS,CountyName,Year,LCName) %>%
                              summarise(Total_Area_SqMiles = sum(SquareMiles)) %>% spread(LCName,Total_Area_SqMiles) %>%
                              rename(High_Intensity_Developed_SqMiles  = `High Intensity Developed`,
                                     Medium_Intensity_Developed_SqMiles  = `Medium Intensity Developed`,
                                     Low_Intensity_Developed_SqMiles  = `Low Intensity Developed`)))
base10 <- read_csv(paste0(prefix,'LandCover2010.csv')) %>% filter(StateAbbrev == 'FL')
developed_10 <- Reduce(full_join,
                       list(base10 %>% group_by(FIPS,CountyName,Year) %>%
                              summarise(Total_Area_SqMiles = sum(SquareMiles)),
                            base10 %>% 
                              filter(grepl('Intensity Developed',LCName)) %>% group_by(FIPS,CountyName,Year) %>%
                              summarise(Developed_Area_SqMiles = sum(SquareMiles)),
                            base10 %>% 
                              filter(grepl('Intensity Developed',LCName)) %>% group_by(FIPS,CountyName,Year,LCName) %>%
                              summarise(Total_Area_SqMiles = sum(SquareMiles)) %>% spread(LCName,Total_Area_SqMiles) %>%
                              rename(High_Intensity_Developed_SqMiles  = `High Intensity Developed`,
                                     Medium_Intensity_Developed_SqMiles  = `Medium Intensity Developed`,
                                     Low_Intensity_Developed_SqMiles  = `Low Intensity Developed`)))



change9601 <- read_csv(paste0(prefix,'LandCoverChange_96_01.csv')) %>% filter(StateAbbrev == 'FL') 
change0106 <- read_csv(paste0(prefix,'LandCoverChange_01_06.csv')) %>% filter(StateAbbrev == 'FL') 
change0610 <- read_csv(paste0(prefix,'LandCoverChange_06_10.csv')) %>% filter(StateAbbrev == 'FL') 
change <- Reduce(full_join,list(change9601,change0106,change0610))
new_developed_area <- change %>% filter(grepl('Developed$',toClass),!grepl('Developed$',fromClass)) %>% 
  group_by(FIPS,CountyName,FromYear,ToYear) %>% summarise(New_Developed_Area = sum(SquareMiles))
new_reconverted_area <- change %>% filter(!grepl('Developed$',toClass),grepl('Developed$',fromClass)) %>% group_by(FIPS,CountyName,FromYear,ToYear) %>% 
  summarise(New_Reconverted_Area = sum(SquareMiles)) 

county_change <- full_join(new_developed_area,new_reconverted_area) %>% mutate(Net_Change_In_Developed_Area = New_Developed_Area - New_Reconverted_Area)

write.csv(county_change,'fl_dev_change_by_county')

library(rgdal)
library(rgeos)
fl_counties <- readOGR(prefix,'county_nrcs_a_fl')
fl_places <- readOGR(prefix,'tl_2016_12_place')
library(raster)

library("sf")
library("magrittr")
prj <- '+proj=laea +lat_0=10 +lon_0=-81 +ellps=WGS84 +units=m +no_defs'
sp1 <- shapefile(paste0(prefix,'tl_2016_us_county')) %>%
  spTransform(CRS(prj)) %>%
  gBuffer(byid=TRUE, width=0)
sp1 <- sp1[sp1@data$STATEFP == '12',]
for (i in 1:length(sp1))
{sp1@polygons[[i]]@ID <- paste0('12',sp1@data$COUNTYFP[i])}

sp2 <- shapefile(paste0(prefix,'tl_2016_12_place.shp')) %>%
  spTransform(CRS(prj)) %>%
  gBuffer(byid=TRUE, width=0)
sp2 <- sp2[!grepl('CDP',sp2@data$NAMELSAD),]
sp3 <- gIntersection(sp1, sp2,byid = T,drop_lower_td = T)
ol <- gArea(sp3,byid = T)

overs = do.call(rbind,stringr::str_split(names(ol),' ')) %>% as.data.frame() %>%
  mutate(area = as.vector(ol)) %>% rename(FIPS = V1)

over_df = overs %>% group_by(FIPS) %>% summarise(over_place = sum(area))
areas <- data.frame(FIPS = names(gArea(sp1,byid=T)),AREA = as.vector(gArea(sp1,byid=T)))
over_df = left_join(over_df,areas)
over_df <- over_df %>% mutate(area_in_census_place = over_place/AREA)

over_df$NAME <- sp1@data$NAME[match(over_df$FIPS,paste0('12',sp1@data$COUNTYFP))]

incorps = overs %>% group_by(FIPS) %>% summarise(incorporated_places = n())
over_df = left_join(over_df,incorps)

over_df$ALAND <- as.numeric(sp1@data$ALAND[match(over_df$FIPS,paste0('12',sp1@data$COUNTYFP))])
over_df$AWATER <- as.numeric(sp1@data$AWATER[match(over_df$FIPS,paste0('12',sp1@data$COUNTYFP))])
over_df$PROP_WATER_AREA <- over_df$AWATER / (over_df$ALAND + over_df$AWATER)

over_df <- over_df %>% rename(PROP_AREA_OVER_INCORPORATED_PLACE =  area_in_census_place,
                              AREA_OVER_INCORPORATED_PLACE = over_place,
                              NUM_INCORPORATED_PLACES = incorporated_places)

write_csv(over_df,'fl_county_area_stats.csv')







