
library(tidyverse)
library(rgdal);library(rgeos)
#cdd_codes <- read_csv('spatial_input/county_parcel/pasco/pasco_cdd_tax_codes.csv')

css_auth = '.hdr:nth-child(4) , .data~ .data+ .data div , .hdr:nth-child(5) , .data:nth-child(1) div , .hdr:nth-child(1)'
css_dist = '.data:nth-child(2) div , .hdr:nth-child(2) , .data:nth-child(1) div , .hdr:nth-child(1)'
url_authorities = 'http://search.pascopa.com/millage.aspx?pid=dist&year=2016'
url_areacodes = 'http://search.pascopa.com/millage.aspx?pid=area&year=2016'
url_areadoces2 = 'http://search.pascopa.com/millage.aspx?mprs=2&src=Q&pid=area&year=2016&sf=0&so=1&recs=100&pg=2'

library(rvest)
millage = url_authorities %>% read_html() %>% html_nodes(css = css_auth) %>% html_text(trim=T) 
mill_mat = as.data.frame(matrix(millage,byrow=T,ncol=3))
colnames(mill_mat) <- c('AUTH_CODE','Abbreviation','AUTH_NAME')
mill_mat <- mill_mat[-1,]
mill_mat <- mill_mat[grepl('CDD|COMM DEV DIST|COMMUNITY DEV DIST',mill_mat$AUTH_NAME),]
dist = url_areacodes %>% read_html() %>% html_nodes(css = css_dist) %>% html_text(trim=T)
dist_mat = as.data.frame(matrix(dist,byrow=T,ncol=2))
colnames(dist_mat) <- c('DIST_CODE','AUTH_COMBOS')
dist_mat <- dist_mat[-1,]
dist_code_ref <- data.frame(do.call(rbind,mapply(function(x,y) cbind(unlist(str_split(x,pattern = '-')),y),
       as.character(dist_mat$AUTH_COMBOS),as.character(dist_mat$DIST_CODE),SIMPLIFY = F)))
colnames(dist_code_ref) <- c('AUTH_CODE','DIST_CODE')
dist_code_ref <- left_join(dist_code_ref,mill_mat)
dist_code_ref <- dist_code_ref[!is.na(dist_code_ref$AUTH_NAME),]
pasco_parcels <- readOGR('spatial_input/county_parcel/pasco','pasco_fix')
#pasco_cdd_parcels <- pasco_parcels[pasco_parcels@data$AREA_CODE %in% dist_code_ref$DIST_CODE,]
pasco_parcels@data$CDD_NAME <- dist_code_ref$AUTH_NAME[match(pasco_parcels@data$AREA_CODE,dist_code_ref$DIST_CODE)]
pasco_cdd_parcels = pasco_parcels[!is.na(pasco_parcels@data$CDD_NAME),]
library(pbapply)
pasco_cdds <- gUnaryUnion(pasco_cdd_parcels,id = as.character(pasco_cdd_parcels@data$CDD_NAME))
pasco_df <- data.frame(CDD_NAME = getSpPPolygonsIDSlots(pasco_cdds))
rownames(pasco_df) <- pasco_df$CDD_NAME
pasco_cdd_spdf <- SpatialPolygonsDataFrame(pasco_cdds,data = pasco_df)
writeOGR(pasco_cdd_spdf, "spatial_input/county_cdd", "FL_Pasco_CDD", driver="ESRI Shapefile",overwrite_layer = TRUE)


