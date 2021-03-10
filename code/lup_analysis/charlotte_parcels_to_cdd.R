library(rgdal)
library(tidyverse)
library(stringr)
library(rgeos)
ca = readOGR('spatial_input/county_parcel/charlotte','ACCOUNTS')
charlotte_codes = read_csv('spatial_input/county_parcel/charlotte/Charlotte_County_Tax_District_Codes.csv')

parcels = read_delim('spatial_input/county_parcel/charlotte/cd.txt',delim = '|',col_names = FALSE) 
colnames(parcels)[14] <- 'TAX_CODE'
colnames(parcels)[1] <- 'PIN'
parcels$PIN = gsub('\\s{1,}','',parcels$PIN)

cdds_in_county <- charlotte_codes[charlotte_codes$Variable == 'Authority'&grepl('CDD',charlotte_codes$Authority_Name),]
authority_codes <- charlotte_codes[charlotte_codes$Variable == 'Code',]
splits <- stringr::str_split(authority_codes$Authority_Name,pattern = '\\+')
authority_by_code <- data.frame(do.call(rbind,mapply(function(x,y) cbind(unit = gsub('\\s','',x),code = rep(y,length(x))), splits,authority_codes$Authority_Code)))
colnames(authority_by_code) <- c('Authority_Code','Tax_Code')
cdds_in_county <- left_join(cdds_in_county,authority_by_code)
cdd_parcels <- parcels[parcels$TAX_CODE %in% cdds_in_county$Tax_Code,]
cdd_parcels$CDD_NAME <- cdds_in_county$Authority_Name[match(cdd_parcels$TAX_CODE,cdds_in_county$Tax_Code)]

cdd_parcel_map = ca[as.character(ca@data$PIN) %in% cdd_parcels$PIN,]
cdd_parcel_map@data$CDD_NAME <- cdd_parcels$CDD_NAME[match(cdd_parcel_map@data$PIN,cdd_parcels$PIN)]
library(rgeos)
charlotte_cdds <- gUnaryUnion(cdd_parcel_map,id = cdd_parcel_map@data$CDD_NAME)
dff <- data.frame(CDD_NAME = sapply(charlotte_cdds@polygons,function(x) x@ID))
rownames(dff) <- dff$CDD_NAME
charlotte_cdds_spdf <- SpatialPolygonsDataFrame(charlotte_cdds,data = dff)
writeOGR(charlotte_cdds_spdf, "spatial_input/county_cdd", "FL_Charlotte_CDD", driver="ESRI Shapefile",overwrite_layer = TRUE)

