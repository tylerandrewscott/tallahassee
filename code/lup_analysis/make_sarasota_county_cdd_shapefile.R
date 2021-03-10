sarasota_cid<-readOGR('spatial_input/county_cdd/sarasota_subs',"CDD_CID_STEWARDSHIP_DISTRICTS")
sarasota_cdd<-sarasota_cid[grepl('CDD',sarasota_cid@data$NOTES),]
sarasota_cdd@data$CDD_NAME<-as.character(sarasota_cdd@data$NOTES)
sarasota_cdd@data$CDD_NAME[sarasota_cdd@data$CDD_NAME=="MYAKKA RANCH CDD NOTE OF ESTABLISHMNET ORI 2008035515"]<-"MYAKKA RANCH CDD"
writeOGR(sarasota_cdd, "spatial_input/county_cdd", "FL_Sarasota_CDD", driver="ESRI Shapefile",overwrite_layer = TRUE)

