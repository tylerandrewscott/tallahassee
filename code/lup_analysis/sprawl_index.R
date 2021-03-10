

library(tidyverse)
library(rgdal)
library(maptools)
library(rgeos)
library(raster)
library(maps)
library(SDMTools)
library(doParallel)  #Foreach Parallel Adaptor 
library(foreach)     #Provides foreach looping construct

#All Development as of 2001
#Low Intensity Development as of 2001
#Clumpiness of All Development as of 2001
#Clumpiness of Low-intensity Development in 2001
#2001 Impervious Cover per Capita
#Population per Acre (Density)
#Change in Population Density

library(tidyverse)
#prefix = 'input'
prefix = 'input/ccap_summary/'
base96 <- read_csv(paste0(prefix,'LandCover1996.csv')) #%>% filter(StateAbbrev == 'FL')
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


base01 <- read_csv(paste0(prefix,'LandCover2001.csv')) 
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
base06 <- read_csv(paste0(prefix,'LandCover2006.csv')) 
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
base10 <- read_csv(paste0(prefix,'LandCover2010.csv'))
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

counties = readOGR('spatial_input/government_units/','tl_2016_us_county')
counties@data$Square_Miles = {as.numeric(as.character(counties@data$ALAND))+
    as.numeric(as.character(counties@data$AWATER))} * 0.00000038610
counties@data$FIPS = paste0(counties@data$STATEFP,counties@data$COUNTYFP)

sprawl_df = Reduce(full_join,lapply(grep('developed_',ls(),value=T),get))

sprawl_df <- sprawl_df %>% mutate(All_Developed_Area = Developed_Area_SqMiles/Total_Area_SqMiles,
                                  Low_Intensity_Developed_Area = Low_Intensity_Developed_SqMiles/Total_Area_SqMiles)
sprawl_df <- sprawl_df %>% filter(CountyName!='District of Columbia')

sprawl_df$Square_Miles <- counties@data$Square_Miles[match(sprawl_df$FIPS,counties@data$FIPS)]

library(pbapply)
rasterOptions(maxmemory = 1e+09)
#Define how many cores you want to use
#UseCores <- detectCores() * 0.5
UseCores <- 20
#Register CoreCluster
pref <- 'spatial_input/County_CCAPS/'
fls <- list.files('spatial_input/County_CCAPS/',pattern = '.grd')

#cl <- makeCluster(UseCores)
#registerDoParallel(cl)

dev_list <- foreach(i=1:nrow(sprawl_df)) %do% {
  print(i)
  library(raster);library(SDMTools)
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
  mults = c(0.10,0.35,0.65,0.90)
cat_dev_class_matrix = rbind(cbind(c(0:1),NA),cbind(c(2:5),2:5),cbind(c(6:25),0))
all_dev_class_matrix = rbind(cbind(c(0:1),NA),cbind(c(2:5),1),cbind(c(6:25),0))
low_dev_class_matrix = rbind(cbind(c(0:1),NA),cbind(c(2:3),0),cbind(c(4),1),cbind(c(5:25),0))
r = raster(paste0(pref,fls[which(grepl(sprawl_df$FIPS[i],fls)&grepl(paste0('_',sprawl_df$Year[i],'_'),fls))]))
r_cat_dev <- reclassify(r,rcl = cat_dev_class_matrix)
r_all_dev <- reclassify(r,rcl = all_dev_class_matrix)
r_low_dev <- reclassify(r,rcl = low_dev_class_matrix)
class_low_dev <- ClassStat(r_low_dev,cellsize = 1)
class_all_dev <- ClassStat(r_all_dev,cellsize = 1)
class_cat_dev <- ClassStat(r_cat_dev,cellsize = 1)
All_Dev_CLUMPY = compute_clumpy(class_all_dev)
Low_Dev_CLUMPY = compute_clumpy(class_low_dev)
Imperv_Area = sum(class_cat_dev$total.area[class_cat_dev$class!=0]*mults)
return(data.frame(All_Dev_CLUMPY,Low_Dev_CLUMPY,Imperv_Area,FIPS = sprawl_df$FIPS[i],Year = sprawl_df$Year[i]))}

#stopCluster(cl)

dev_df = do.call(rbind,dev_list)

sprawl_df <- left_join(sprawl_df,dev_df)
gc()
#0-20,21-49,50-79,80-100
p1 = read_csv('input/census/PEP_2016_PEPANNRES_with_ann.csv',skip=1) %>% dplyr::select(-Id,-Geography) %>%
  gather(Year,Est,-Id2) %>% mutate(Year = str_extract(Year,'[0-9]{4}')) %>% rename(FIPS = Id2) %>% filter(!is.na(FIPS))
p2 = read_csv('input/census/co-est00int-tot.csv',skip=0) %>% mutate(STATE = sprintf("%02i",STATE),
                                                                    COUNTY = sprintf("%03i",COUNTY),
                                                                    FIPS = paste0(STATE,COUNTY)) %>%
  filter(COUNTY!='000') %>%
  dplyr::select(-SUMLEV,-REGION,-DIVISION,-STNAME,-CTYNAME,-STATE,-COUNTY,-ESTIMATESBASE2000 ) %>%
  gather(Year,Est,-FIPS) %>% mutate(Year = str_extract(Year,'[0-9]{4}'))

#Year
#FIPS code state
#FIPS code county
#Age Group
#Race-Sex
#Ethnic origin
#POP

p3 = read_table('input/census/stch-icen1996.txt',col_names = F) %>% rename(Year = X1,FIPS = X2,Pop = X6) %>% 
  mutate(Year = paste0('19',Year)) %>%
  group_by(Year,FIPS) %>% summarise(Est = sum(Pop))
pops = Reduce(full_join,list(p1,p2,p3))

sprawl_df = left_join(sprawl_df,pops %>% mutate(Year = as.numeric(Year)) %>% rename(Pop = Est))
sprawl_df$Square_Miles = counties@data$Square_Miles[match(sprawl_df$FIPS,counties@data$FIPS)]
sprawl_df$Pop_Density = sprawl_df$Pop/sprawl_df$Square_Miles
write.csv(sprawl_df,'scratch/sprawl_data.csv')

temp = read_csv('scratch/sprawl_data.csv') %>% dplyr::select(-X1)

temp_fl <- temp %>% filter(grepl('^12',FIPS))
temp_fl$I1_Dev_Area_Prop_FL <- scale(log(temp_fl$Developed_Area_SqMiles/temp_fl$Total_Area_SqMiles))
temp_fl$I2_Low_Dev_Area_Prop_FL <- scale(log(temp_fl$Low_Intensity_Developed_SqMiles/temp_fl$Total_Area_SqMiles))
temp_fl$I3_All_Dev_Clustering_FL <- scale(log(temp_fl$All_Dev_CLUMPY))
temp_fl$I4_Low_Dev_Clustering_FL <- scale(log(temp_fl$Low_Dev_CLUMPY))
temp_fl$I5_Imperv_Area_Per_Capita_FL <- scale(log(temp_fl$Imperv_Area/temp_fl$Pop))
temp_fl$I6_Pop_Density_FL <- scale(log(temp_fl$Pop_Density))
temp_fl$Sprawl_Index_FL_Scale <- rowSums(temp_fl[,grepl('^I[0-9]',names(temp_fl))])

temp$I1_Dev_Area_Prop <- scale(log(temp$Developed_Area_SqMiles/temp$Total_Area_SqMiles))
temp$I2_Low_Dev_Area_Prop <- scale(log(temp$Low_Intensity_Developed_SqMiles/temp$Total_Area_SqMiles))
temp$I3_All_Dev_Clustering <- scale(log(temp$All_Dev_CLUMPY))
temp$I4_Low_Dev_Clustering <- scale(log(temp$Low_Dev_CLUMPY))
temp$I5_Imperv_Area_Per_Capita <- scale(log(temp$Imperv_Area/temp$Pop))
temp$I6_Pop_Density <- scale(log(temp$Pop_Density))
temp$Sprawl_Index_US_Scale <- rowSums(temp[,grepl('^I[0-9]',names(temp))])

temp <- full_join(temp,temp_fl) 

temp <- temp %>% filter(grepl('^12',FIPS))

write.csv(temp,'input/sprawl_dev_indices_v2.csv')


