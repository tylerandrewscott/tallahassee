library(sp)
library(rgdal)
library(raster)
library(SDMTools)
library(maptools)
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
fl = readOGR('spatial_input/government_units/','county_nrcs_a_fl')
shape_files = unique(gsub('\\..*','',grep('CDD',list.files('spatial_input/county_cdd'),value=T)))
prefix = 'spatial_input/county_cdd'
#manatee county
cdd_manatee = spTransform(readOGR(prefix,'FL_Manatee_CDD'),CRS(proj4string(fl)))
cdd_manatee@data$COUNTY = 'manatee'
cdd_manatee@data$CDD_NAME = toupper(cdd_manatee@data$CDD_NAME)
cdd_manatee@data$CDD_NAME[cdd_manatee@data$CDD_NAME == "ROBION AT THE VILLAGES OF AVI*"] <- "ROBION AT THE VILLAGES OF AVIGNON"
avig = cdd_manatee[grepl('AVIGNON',cdd_manatee$CDD_NAME),]
cdd_manatee = cdd_manatee[!grepl('AVIGNON',cdd_manatee$CDD_NAME),]
tdf <- avig@data[1,]
tdf$CDD_NAME = 'VILLAGES OF AVIGNON'
row.names(tdf) = 1:nrow(tdf)
ids = length(avig)
avig = SpatialPolygonsDataFrame(unionSpatialPolygons(avig,IDs = rep(1,ids)),data=tdf)
avig = spChFIDs(avig,'100')
cdd_manatee = spRbind(cdd_manatee,avig)
#st lucie county
cdd_stlucie = spTransform(readOGR(prefix,'FL_StLucie_CDD'),CRS(proj4string(fl)))
cdd_stlucie@data = cdd_stlucie@data %>% rename(CDD_NAME = Name)
cdd_stlucie@data$COUNTY = 'stlucie'
#miami dade county
cdd_dade = spTransform(readOGR(prefix,'FL_Dade_CDD'),CRS(proj4string(fl)))
cdd_dade@data = cdd_dade@data %>% rename(CDD_NAME = NAME)
cdd_dade@data$COUNTY = 'miamidade'
#lee county
cdd_lee = spTransform(readOGR(prefix,'FL_Lee_CDD'),CRS(proj4string(fl)))
cdd_lee@data = cdd_lee@data %>% rename(CDD_NAME = Name)
cdd_lee@data$COUNTY = 'lee'

#charlotte county
cdd_charlotte = spTransform(readOGR(prefix,'FL_Charlotte_CDD'),CRS(proj4string(fl)))
cdd_charlotte@data$COUNTY = 'charlotte'

#pasco county
cdd_pasco = spTransform(readOGR(prefix,'FL_Pasco_CDD'),CRS(proj4string(fl)))
cdd_pasco@data$COUNTY = 'pasco'

#duval county
cdd_duval = spTransform(readOGR(prefix,'FL_Duval_CDD'),CRS(proj4string(fl)))
cdd_duval@data = cdd_duval@data %>% rename(CDD_NAME = CDDName)
cdd_duval@data$COUNTY = 'duval'
#leon county
cdd_leon = spTransform(readOGR(prefix,'FL_Leon_CDD'),CRS(proj4string(fl)))
cdd_leon@data = cdd_leon@data %>% rename(CDD_NAME = CDD)
cdd_leon@data$COUNTY = 'leon'
#osceola county
cdd_osceola = spTransform(readOGR(prefix,'FL_Osceola_CDD'),CRS(proj4string(fl)))
cdd_osceola@data = cdd_osceola@data %>% rename(CDD_NAME = Layer)
cdd_osceola@data$COUNTY = 'osceola'
#flagler county
cdd_flagler = spTransform(readOGR(prefix,'FL_Flagler_CDD'),CRS(proj4string(fl)))
cdd_flagler@data = cdd_flagler@data %>% rename(CDD_NAME = GIS_FIG)
cdd_flagler@data$COUNTY = 'flagler'

#clay county
cdd_clay = spTransform(readOGR(prefix,'FL_Clay_CDD'),CRS(proj4string(fl)))
cdd_clay@data = cdd_clay@data %>% rename(CDD_NAME = Name)
cdd_clay@data$COUNTY = 'clay'

#hernando county
cdd_hernando = spTransform(readOGR(prefix,'FL_Hernando_CDD'),CRS(proj4string(fl)))
cdd_hernando@data = cdd_hernando@data %>% rename(CDD_NAME = NAME)
cdd_hernando@data$COUNTY = 'hernando'

#orange county
cdd_orange = spTransform(readOGR(prefix,'FL_Orange_CDD'),CRS(proj4string(fl)))
cdd_orange@data = cdd_orange@data %>% rename(CDD_NAME = NAME)
cdd_orange@data$COUNTY = 'orange'
#polk county
cdd_polk = spTransform(readOGR(prefix,'FL_Polk_CDD'),CRS(proj4string(fl)))
cdd_polk@data = cdd_polk@data %>% rename(CDD_NAME = Name)
cdd_polk@data$COUNTY = 'polk'
#st johns county
cdd_stjohns = spTransform(readOGR(prefix,'FL_StJohns_CDD'),CRS(proj4string(fl)))
cdd_stjohns@data = cdd_stjohns@data %>% rename(CDD_NAME = NAME)
cdd_stjohns@data$COUNTY = 'stjohns'

#sarasota county=
cdd_sarasota = spTransform(readOGR(prefix,'FL_Sarasota_CDD'),CRS(proj4string(fl)))
cdd_sarasota@data$COUNTY = 'sarasota'

#hillsborough county
cdd_hillsborough = spTransform(readOGR(prefix,'FL_Hillsborough_CDD'),CRS(proj4string(fl)))
cdd_hillsborough@data$COUNTY = 'hillsborough'
#bay county
cdd_bay = spTransform(readOGR(prefix,'FL_Bay_CDD'),CRS(proj4string(fl)))

#nassau county
cdd_nassau = spTransform(readOGR(prefix,'FL_Nassau_CDD'),CRS(proj4string(fl)))
cdd_nassau@data$COUNTY = 'nassau'
cdd_nassau@data = cdd_nassau@data %>% rename(CDD_NAME = Name)

#seminole county
cdd_seminole = spTransform(readOGR(prefix,'FL_Seminole_CDD'),CRS(proj4string(fl)))
cdd_seminole@data$COUNTY = 'seminole'

#join all counties
cdds <- do.call(bind,list(cdd_bay, cdd_charlotte,cdd_clay,cdd_dade,
                          cdd_duval,cdd_flagler,cdd_hernando, cdd_hillsborough,
                          cdd_lee,cdd_leon,cdd_manatee, cdd_nassau, 
                          cdd_orange, cdd_osceola,cdd_pasco, cdd_polk,
                          cdd_sarasota,cdd_seminole,cdd_stjohns,cdd_stlucie))

cdds$CDD_NAME <- toupper(cdds$CDD_NAME)
cdds$CDD_NAME <- gsub('COMMUNIITY','COMMUNITY',cdds$CDD_NAME)
cdds$CDD_NAME <- gsub('DEVELPMENT','DEVELOPMENT',cdds$CDD_NAME)
cdds$CDD_NAME <- gsub(' CDD| COMMUNITY DEVELOPMENT$| COMMUNITY DEVELOPMENT DISTRICT$| COMMUNITY DEV DIST$| COMM DEV DISTRICT$| COMM DEV DIST$','',cdds$CDD_NAME)

cdds$CDD_NAME <- gsub('^THE ','',cdds$CDD_NAME)
cdds$CDD_NAME <- gsub(' I$',' #1',cdds$CDD_NAME)
cdds$CDD_NAME <- gsub(' II$',' #2',cdds$CDD_NAME)
cdds$CDD_NAME <- gsub(' III$',' #3',cdds$CDD_NAME)
cdds$CDD_NAME <- gsub(' IV$',' #4',cdds$CDD_NAME)
cdds$CDD_NAME <- gsub('SOUTHFORK','SOUTH FORK',cdds$CDD_NAME)
cdds$CDD_NAME[cdds$CDD_NAME == "SOUTH SHORE CORP PARK" ] <- "SOUTH SHORE CORPORATE PARK INDUSTRIAL"
cdds$CDD_NAME[cdds$CDD_NAME == "COCO PALM" ] <- "COCO PALMS"
cdds$CDD_NAME[cdds$CDD_NAME == "MIDTOWN ORLANDO" ] <- "URBAN ORLANDO"
cdds$CDD_NAME[cdds$CDD_NAME == "STONEBRIAR" ] <- "STONEBRIER"
cdds$CDD_NAME[cdds$CDD_NAME == "AVENTURE ISLES" ] <- "AVENTURA ISLES"
cdds$CDD_NAME[cdds$CDD_NAME == "SILVER LEAF" ] <- "SILVERLEAF"
cdds$CDD_NAME[cdds$CDD_NAME == "MILLS" ] <- "MILLS PARK"
cdds$CDD_NAME[cdds$CDD_NAME == "TSR (STARKEY RANCH)" ] <- "TSR"
cdds$CDD_NAME[cdds$CDD_NAME == "KINDRED" ] <- "TOWN OF KINDRED"
cdds$CDD_NAME[cdds$CDD_NAME == "MANDARIN GROVES" ] <- "MANDARIN GROVE"
cdds$CDD_NAME[cdds$CDD_NAME == "TRAILS AT MONTERREY" ] <- "TRAILS AT MONTEREY"
cdds$CDD_NAME[cdds$CDD_NAME == "ISLANDS AT DORAL (SW)" ] <- "ISLANDS AT DORAL SW"
cdds$CDD_NAME[cdds$CDD_NAME == "ISLANDS AT DORAL (NE)" ] <- "ISLANDS AT DORAL NE"
cdds$CDD_NAME[cdds$CDD_NAME == "BLUEWATERS SUBDIVISION" ] <- "BLUEWATERS"
cdds$CDD_NAME[cdds$CDD_NAME == "NOCATEE DUVAL" ] <- "TOLOMATO"
cdds$CDD_NAME[cdds$CDD_NAME == "IL VILLIAGIO" ] <- "PRINCIPAL ONE"
cdds$CDD_NAME[cdds$CDD_NAME ==  "LAKEWOOD RANCH"  ] <- "LAKEWOOD RANCH #3"
cdds$CDD_NAME[cdds$CDD_NAME == "CROSS CREEK" ] <- "CROSSCREEK"
cdds$CDD_NAME[cdds$CDD_NAME == "BAYCREEK" ] <- "BAY CREEK"
cdds$CDD_NAME[cdds$CDD_NAME == "TALAVERA PER ORD 06-33" ] <- "TALAVERA"
cdds$CDD_NAME[cdds$CDD_NAME == "WATERGRASS" ] <- "WATERGRASS #1"
cdds$CDD_NAME[cdds$CDD_NAME == "WATERGRASS II" ] <- "WATERGRASS #2"
cdds$CDD_NAME[cdds$CDD_NAME == "OAKS" & cdds@data$COUNTY=='osceola' ] <- 'OVEROAKS'
cdds$CDD_NAME[cdds$CDD_NAME == "HUNTERS RIDGE" ] <- "HUNTERS RIDGE #1"
cdds$CDD_NAME[cdds$CDD_NAME == "GARDENS"& cdds@data$COUNTY=='flagler'] <- "GARDENS AT HAMMOCK BEACH"
cdds$CDD_NAME[cdds$CDD_NAME == "PORTOFINO"&cdds@data$COUNTY=='osceola' ] <- "PORTOFINO VISTA"
cdds$CDD_NAME[cdds$CDD_NAME == "PALM COAST" ] <- "TOWN CENTER AT PALM COAST"
cdds$CDD_NAME[cdds$CDD_NAME == "RIVER PLACE" ] <- "RIVER PLACE ON THE ST LUCIE"
cdds$CDD_NAME[cdds$CDD_NAME == "STERLING HILLS" ] <- "STERLING HILL"
cdds$CDD_NAME[cdds$CDD_NAME == "ZEPHYR LAKE" ] <- "ZEPHYR LAKES"
cdds$CDD_NAME[cdds$CDD_NAME == "BRIDGEWATER OF WESLEY CHAP" ] <- "BRIDGEWATER OF WESLEY CHAPEL"
cdds$CDD_NAME[grepl(' [0-9]{1,2}$',cdds$CDD_NAME)] <- gsub(' (?=[0-9]{1,2})',' #',grep(' [0-9]',cdds$CDD_NAME,value=T),perl = T)
cdds$CDD_NAME <- gsub('\\.','',cdds$CDD_NAME)
cdds$CDD_NAME <- gsub('\\s\\(STREET LIGHTING\\)','',cdds$CDD_NAME)
cdds$CDD_NAME <- gsub(' BOUNDARY$','',cdds$CDD_NAME)
cdds$CDD_NAME <- gsub(' COMMUNITY DEVELOPMENT$','',cdds$CDD_NAME)
cdds$CDD_NAME <- gsub('MARKETPLACE','MARKET PLACE',cdds$CDD_NAME)
cdds$CDD_NAME <- gsub('HARBOR','HARBOUR',cdds$CDD_NAME)
cdds$CDD_NAME[cdds$CDD_NAME == "HUNNINGTON HAMMOCKS" ] <- "HUNTINGTON HAMMOCKS"
cdds$CDD_NAME[cdds$CDD_NAME == "WATER'S EDGE" ] <- "WATERS EDGE"
cdds$CDD_NAME[cdds$CDD_NAME == "MEADOW POINTE" ] <- "MEADOW POINTE #1"
cdds$CDD_NAME[cdds$CDD_NAME == "MEADOW POINT #2" ] <- "MEADOW POINTE #2"

cdds$CDD_NAME[cdds$CDD_NAME == "COUNTRY WALK (MEADOW WOODS)" ] <- "COUNTRY WALK"
cdds$CDD_NAME[cdds$CDD_NAME == "DOUBLE VILLAGE" ] <- "DOUBLE BRANCH"

cdd_admin = read_delim('input/DistrictFunctions.txt','\t') %>% filter(!is.na(`County(ies)`)) %>%
  mutate(CDD_NAME = toupper(gsub(' Community Development District| Community Develpment District','', `District's Name`))) %>%
  rename(COUNTY = `County(ies)`)

county_count = cdd_admin %>% group_by(COUNTY) %>% summarise(co = n()) %>% arrange(-co) 

library(foreign)

cpop = read_csv('input/census/PEP_2016_PEPANNRES_with_ann.csv') %>% filter(grepl('^12',GEO.id2)) %>%
  rename(COUNTY = `GEO.display-label`) %>% mutate(COUNTY = gsub(' County.*','',COUNTY)) %>%
  select(COUNTY,respop72016) 
cpop$respop72016 = as.numeric(cpop$respop72016)
county_count = left_join(county_count,cpop)

ggplot(county_count,aes(x = respop72016,y = co)) + geom_point()




diss = 'http://specialdistrictreports.floridajobs.org/webreports/dissolved.aspx'
library(rvest)
diss_name = diss %>% read_html() %>% html_nodes(css = '#MainContent_DissolvedDistrictsDataList table td:nth-child(1) :nth-child(1)') %>% html_text(trim=TRUE)
diss_type = diss %>% read_html() %>% html_nodes(css = 'td:nth-child(2) :nth-child(1)') %>% html_text(trim=TRUE)
diss_county = diss %>% read_html() %>% html_nodes(css = 'td:nth-child(3) :nth-child(1)') %>% html_text(trim=TRUE)
diss_dissolved = diss %>% read_html() %>% html_nodes(css = 'td:nth-child(4) :nth-child(1)') %>% html_text(trim=TRUE)
diss_df = data.frame(diss_name,diss_county,diss_type,diss_dissolved)
diss_df = diss_df[-1,]

diss_df = diss_df %>% filter(grepl('Community Development District',diss_name)) %>% 
  mutate(CDD_NAME = toupper(gsub(' Community Development District','', diss_name)),
         Year_Deactivated = lubridate::year(lubridate::mdy(diss_dissolved)))  %>%
  dplyr::select(-diss_name,-diss_dissolved) %>% rename(COUNTY = diss_county)

cdd_admin = full_join(cdd_admin,diss_df)


cdd_admin$CDD_NAME[cdd_admin$CDD_NAME == "ISLANDS AT DORAL (SW)" ] <- "ISLANDS AT DORAL SW"
cdd_admin$CDD_NAME[cdd_admin$CDD_NAME == "ISLANDS AT DORAL (NE)" ] <- "ISLANDS AT DORAL NE"
cdd_admin$CDD_NAME[cdd_admin$CDD_NAME == "FOUR SEASONS AT CRYSTAL SPRINGS" ] <- "FOUR SEASONS"
cdd_admin$CDD_NAME = gsub('NO\\. ','#',cdd_admin$CDD_NAME)
cdd_admin$CDD_NAME = gsub(' $','',cdd_admin$CDD_NAME)
cdd_admin$CDD_NAME = gsub("'",'',cdd_admin$CDD_NAME)
cdd_admin$CDD_NAME <- gsub(' I$',' #1',cdd_admin$CDD_NAME)
cdd_admin$CDD_NAME<- gsub(' II$',' #2',cdd_admin$CDD_NAME)
cdd_admin$CDD_NAME<- gsub(' III$',' #3',cdd_admin$CDD_NAME)
cdd_admin$CDD_NAME<- gsub(' IV$',' #4',cdd_admin$CDD_NAME)
cdd_admin$CDD_NAME<- gsub('-','',cdd_admin$CDD_NAME)
#cdd_admin$CDD_NAME <- gsub('^VILLAGES OF ','',cdd_admin$CDD_NAME)
cdd_admin$CDD_NAME <- gsub('\\, THE$','',cdd_admin$CDD_NAME)
cdd_admin$CDD_NAME <- gsub('\\.','',cdd_admin$CDD_NAME)
cdd_admin$CDD_NAME <- gsub(' \\(.*\\)','',cdd_admin$CDD_NAME)
cdd_admin$CDD_NAME <- gsub('SOUTHERN HILLS PLANTATION','SOUTHERN HILLS',cdd_admin$CDD_NAME)
cdd_admin$CDD_NAME <- gsub('HARBOR','HARBOUR',cdd_admin$CDD_NAME)
cdd_admin$CDD_NAME[grepl(' [0-9]{1,2}',cdd_admin$CDD_NAME)] <- 
  gsub(' (?=[0-9]{1,2})',' #',grep(' [0-9]',cdd_admin$CDD_NAME,value=T),perl = T)
cdd_admin$CDD_NAME[cdd_admin$CDD_NAME == "FISHHAWK" ] <- "FISHHAWK #1"
cdd_admin$CDD_NAME[cdd_admin$CDD_NAME == "MEADOW POINTE" ] <- "MEADOW POINTE #1"
cdd_admin$CDD_NAME[cdd_admin$CDD_NAME == "VILLAGES OF BLOOMINGDALE" ] <- "BLOOMINGDALE"
cdd_admin$CDD_NAME[cdd_admin$CDD_NAME ==  "ST LUCIE WEST SERVICES DISTRICT" ] <-  "ST LUCIE WEST"
cdd_admin$CDD_NAME[cdd_admin$CDD_NAME == "CYPRESS CREEK OF HILLSBOROUGH COUNTY" ] <- "CYPRESS CREEK"
cdd_admin$Year_Created <- lubridate::year(lubridate::mdy(cdd_admin$`Date Created / Established`))


library(lubridate)
cdds@data$Year_Created = cdd_admin$Year_Created[match(cdds@data$CDD_NAME,cdd_admin$CDD_NAME)]
cdds@data$Year_Deactivated = cdd_admin$Year_Deactivated[match(cdds@data$CDD_NAME,cdd_admin$CDD_NAME)]
cdds@data$Year_Created[cdds@data$CDD_NAME == 'LAKEWOOD RANCH STEWARDSHIP'] <- 2005
cdds@data$Year_Created[cdds@data$CDD_NAME == 'DOVE POND'] <- 2007
cdds@data$Year_Created[cdds@data$CDD_NAME == 'WHISPERING OAKS'] <- 2006
cdds@data$Year_Created[cdds@data$CDD_NAME == 'LAKEWOOD RANCH #3'] <- 1996
cdds@data$Year_Created[cdds@data$CDD_NAME == 'CORAL TOWN PARK'] <- 2008
cdds@data$Year_Created[cdds@data$CDD_NAME == 'PALM ISLE AT DORAL'] <- 2007
cdds@data$Year_Created[cdds@data$CDD_NAME == 'PALAZZO DEL LAGO'] <- 2006
cdds@data$Year_Created[cdds@data$CDD_NAME == 'GARDENS AT MILLENIA'] <- 2007
cdds@data$Year_Created[cdds@data$CDD_NAME == 'MILLS PARK'] <- 2007
cdds@data$Year_Created[cdds@data$CDD_NAME == 'HICKORY HAMMOCK'] <- 2006
cdds@data$Year_Created[cdds@data$CDD_NAME == 'STONE DAIRY CREEK'] <- 2007
cdds@data$Year_Created[cdds@data$CDD_NAME == 'STONE CREST'] <- 2006
cdds@data$Year_Created[cdds@data$CDD_NAME == 'HERITAGE HARBOUR EAST'] <- 2008
cdds@data$Year_Created[cdds@data$CDD_NAME == 'PALMETTO PINES'] <- 2007
#cdds@data$Year_Created[cdds@data$CDD_NAME == 'IL VILLIAGIO'] <- 
cdds@data$Year_Created[cdds@data$CDD_NAME == 'PARK SQUARE'] <- 2008
cdds@data$Year_Created[cdds@data$CDD_NAME == 'RIVER GLENN'] <- 2005
cdds@data$Year_Created[cdds@data$CDD_NAME == "HANOVER LAKES"] <- 2017
cdds@data$Year_Created[cdds@data$CDD_NAME == "MERRICK PARK"] <- 2006
cdds@data$Year_Created[cdds@data$CDD_NAME == "ALEXEN"] <- 2006
cdds@data$Year_Created[cdds@data$CDD_NAME == "LAGUNA ESTATES"] <- 2006
cdds@data$Year_Created[cdds@data$CDD_NAME == "BEACH ROAD ESTATES"] <- 2008
cdds@data$Year_Created[cdds@data$CDD_NAME == "WILLFORD PRESERVE"] <- 2017
cdds@data$Year_Created[cdds@data$CDD_NAME == "HUNTINGTON HAMMOCKS"] <- 2010

cdds@data <- cdds@data[,c('CDD_NAME','Acres','COUNTY','Year_Created','Year_Deactivated')]

out_cbsa <- tolower(c('Walton','Jackson','Suwannee','Levy','Bradford',
                      'Washington','Taylor','Holmes','Madison','Dixie',
                      'Gulf','Union','Hamilton','Calhoun','Glades','Franklin',
                      'Lafayette','Liberty'))
cdds@data$in_cbsa = (!cdds@data$COUNTY %in% out_cbsa) + 0

writeOGR(cdds, "spatial_input/created_inputs", "master_cdd_dataset", driver="ESRI Shapefile",overwrite_layer = TRUE)

