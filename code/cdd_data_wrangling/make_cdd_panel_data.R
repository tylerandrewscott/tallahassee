library(data.table)


#gaz = fread('input/uscb_gazetteer/2018_Gaz_place_national.txt')
#gaz = gaz[USPS=='FL',]

rev = fread('input/florida_lgfr/florida_lg_revenue_1993-2018.csv')
rev = setnames(rev,'DESCRIPTION','ACCOUNT_DESCRIPTION')
# city IDs start with 2, SD IDs start with 3, counties start with 1, other with 5
rev = rev[grepl('^(2|3)',rev$ENTITY_ID),]
#rev = rev[grepl('CDD|COMMUNITY DEVELOPMENT DIST',toupper(ENTITY_NAME)),]
replace_commas = data.table(apply(rev[,GENERAL:COMPONENT_UNITS],2,function(x) as.numeric(gsub("\\,",'',x))))
rev[,10:19] = replace_commas
rev[,Year:=NULL]
rev$TOTAL_REVENUE = rowSums(rev[,10:19],na.rm=T)
#rev[Gov_Type == 'City'|grepl('CDD|Community Development District',ENTITY_NAME),]
library(forcats)
rev$ENTITY_NAME = gsub('\\s{1,}',' ',rev$ENTITY_NAME)
rev$ENTITY_NAME = gsub('Comm\\.','Community',rev$ENTITY_NAME)
rev$ENTITY_NAME = gsub('Developoment|Deveopment|Dvlpmnt|Develoment|Developmente','Development',rev$ENTITY_NAME)
rev$ENTITY_NAME = gsub('CommunityDevelopment Dist\\.|Community(\\s|\\.)Develop\\. Dist\\.|Community(\\s|\\.)Develop(\\s|\\.)District|Community Dev(\\.\\s|\\s)Dist(\\.|$|\\s)|Community(\\s|\\.)Development(\\s|\\.)District|Community(\\s|\\.)Development(\\s|\\.)Dist(\\.$|$|\\s)|Community Develop\\. District|Community Dev\\. District|Community(\\s|\\.)Develop(\\s|\\.)Dist($|\\s)|Community(\\s|\\.)District$|Community(\\s|\\.)Development(\\s|\\.)Distr$','CDD',rev$ENTITY_NAME)
rev$ENTITY_NAME[rev$ENTITY_NAME=="Crossings at Fleming Island CDD, The"] <- "Crossings at Fleming Island CDD"
rev = rev[grepl("CDD",ENTITY_NAME)|Gov_Type=='City',]
rev_comb = rev[,sum(TOTAL_REVENUE),by = .(ENTITY_ID,ENTITY_NAME,YEAR,ACCOUNT_CD,SPEC_CAT_ID,GEN_CAT_ID,Gov_Type)]
rev_2018 = rev[YEAR==2018,]
rev_comb$SPEC_CAT_DESC = rev_2018$SPEC_CAT_DESC[match(rev_comb$SPEC_CAT_ID,rev_2018$SPEC_CAT_ID)]
rev_comb$GEN_CAT_DESC = rev_2018$GEN_CAT_DESC[match(rev_comb$GEN_CAT_ID,rev_2018$GEN_CAT_ID)]
rev_comb$ACCOUNT_DESCRIPTION = rev_2018$ACCOUNT_DESCRIPTION[match(rev_comb$ACCOUNT_CD,rev_2018$ACCOUNT_CD)]
rev_comb = setnames(rev_comb,'V1','TOTAL_REVENUE')

rev_comb$GEN_CAT_DESC = fct_other(rev_comb$GEN_CAT_DESC, drop = c("State Grants","Federal Grants" ,"State Shared","Federal Payments in Lieu of Taxes", "State Payments in Lieu of Taxes" ),other_level = 'State and Federal Grants and Payments')
rev_comb$GEN_CAT_DESC = fct_other(rev_comb$GEN_CAT_DESC,drop = c("Sales","Interest and Other Earnings","Other Sources-Transfers" ,"Judgments, Fines and Forfeits" ,"Court-Related Revenues" , "Miscellaneous Revenues"),other_level = 'Other Sources')
rev_comb$GEN_CAT_DESC = fct_other(rev_comb$GEN_CAT_DESC,drop = c("Local Government Unit Grants","Shared Local Government Unit Grants",'Local Payments in Lieu of Taxe' ),other_level = 'Local Grants and Payments')
rev_comb$GEN_CAT_DESC[is.na(rev_comb$GEN_CAT_DESC)]<- 'Permits, Fees and Licenses'

rev_tot = rev_comb[,sum(TOTAL_REVENUE),by = .(ENTITY_ID,ENTITY_NAME,YEAR,GEN_CAT_DESC)]
setnames(rev_tot,'V1','REVENUE')
rev_tot$GEN_CAT_DESC = paste0('REVENUE ',rev_tot$GEN_CAT_DESC)
rev_cast = dcast(rev_tot,ENTITY_ID + ENTITY_NAME + YEAR  ~ GEN_CAT_DESC,value.var = 'REVENUE',fill  = 0)

exp = fread('input/florida_lgfr/florida_lg_expenditure_1993-2018.csv')
exp = exp[grepl('^(2|3)',exp$ENTITY_ID),]
exp = setnames(exp,'DESCRIPTION','ACCOUNT_DESCRIPTION')
exp[,Year:=NULL]
#exp = exp[grepl('CDD|COMMUNITY DEVELOPMENT DIST',toupper(ENTITY_NAME)),]
exp[,7:16] =data.table(apply(exp[,GENERAL:COMPONENT_UNITS],2,function(x) as.numeric(gsub("\\,",'',x))))
exp$TOTAL_EXPENDITURE = rowSums(exp[,7:16],na.rm=T)
exp$ENTITY_NAME = gsub('\\s{1,}',' ',exp$ENTITY_NAME)
exp$ENTITY_NAME = gsub('Comm\\.','Community',exp$ENTITY_NAME)
exp$ENTITY_NAME = gsub('Developoment|Deveopment|Dvlpmnt|Develoment|Developmente','Development',exp$ENTITY_NAME)
exp$ENTITY_NAME = gsub('Comm Dev Dist|Comm Development Dist|Commnty Dev Dist|CommunityDevelopment Dist\\.|Community(\\s|\\.)Develop\\. Dist\\.|Community(\\s|\\.)Develop(\\s|\\.)District|Community Dev(\\.\\s|\\s)Dist(\\.|$|\\s)|Community(\\s|\\.)Development(\\s|\\.)District|Community(\\s|\\.)Development(\\s|\\.)Dist(\\.$|$|\\s)|Community Develop\\. District|Community Dev\\. District|Community(\\s|\\.)Develop(\\s|\\.)Dist($|\\s)|Community(\\s|\\.)District$|Community(\\s|\\.)Development(\\s|\\.)Distr$','CDD',exp$ENTITY_NAME)

exp_comb = exp[,sum(TOTAL_EXPENDITURE),by = .(ENTITY_ID,ENTITY_NAME,YEAR,ACCOUNT_CD,EXP_FUNCTION_CD,OBJECT_CD)]
exp_2018 = exp[YEAR==2018,]

exp_comb$EXP_FUNCTION_DESC = exp_2018$EXP_FUNCTION_DESC[match(exp_comb$EXP_FUNCTION_CD,exp_2018$EXP_FUNCTION_CD)]
exp_comb$OBJECT_CD_DESC = exp_2018$OBJECT_CD_DESC[match(exp_comb$OBJECT_CD,exp_2018$OBJECT_CD)]
exp_comb$ACCOUNT_DESCRIPTION = exp_2018$ACCOUNT_DESCRIPTION[match(exp_comb$ACCOUNT_CD,exp_2018$ACCOUNT_CD)]
exp_comb = setnames(exp_comb,'V1','TOTAL_REVENUE')


library(forcats)
exp$EXP_CAT = as.factor(exp$EXP_FUNCTION_DESC)
exp$EXP_CAT = fct_other(exp$EXP_CAT,drop = c("General Court Administration","General Court Operations" ,"Circuit Court-Civil","Circuit Court-Family" ,"Circuit Court-Juvenile" ,"Circuit Court-Criminal","County Court-Criminal","County Court-Civil" ,"Circuit Court-Probate","County Court-Traffic"),other_level = 'Other Uses')
exp$EXP_CAT = fct_other(exp$EXP_CAT, drop = c("State Grants","Federal Grants" ,"State Shared","Federal Payments in Lieu of Taxes", "State Payments in Lieu of Taxes" ),other_level = 'State and Federal Grants and Payments')
exp$EXP_CAT = fct_other(exp$EXP_CAT, drop = c("Local Payments in Lieu of Taxes","Local Government Unit Grants","Shared Local Government Unit Grants" ),other_level = 'Local Grants and Payments')

exp_tot = exp[,sum(TOTAL_EXPENDITURE),by = .(ENTITY_ID,ENTITY_NAME,YEAR,EXP_CAT)]
setnames(exp_tot,'V1','EXPENDITURE')
exp_tot$EXP_CAT = paste0('EXPENDITURE ',exp_tot$EXP_CAT)
exp_cast = dcast(exp_tot,ENTITY_ID + ENTITY_NAME + YEAR  ~ EXP_CAT,value.var = 'EXPENDITURE',fill = 0)

exp_cast = exp_cast[ENTITY_ID %in% rev$ENTITY_ID,]
setkey(exp_cast,ENTITY_ID,ENTITY_NAME,YEAR)
setkey(rev_cast,ENTITY_ID,ENTITY_NAME,YEAR)

spend_cast = rev_cast[exp_cast,]
spend_cast$ENTITY_NAME[spend_cast$ENTITY_NAME == 'Lakewood Ranch CDD3']<-  'Lakewood Ranch CDD 3'

tot = fread('input/florida_lgfr/florida_lg_totrevexpdebt_1993-2018.csv')
tot = tot[grepl('^(2|3)',tot$EntityID2),]
names(tot) <- toupper(names(tot))
#tot = tot[grepl('CDD|COMMUNITY DEVELOPMENT DIST',toupper(ENTITYNAME2)),]
setnames(tot,'ENTITYID2','ENTITY_ID')
setnames(tot,'ENTITYNAME2','ENTITY_NAME')
tot$TOTALREV2 <- gsub('\\,','',tot$TOTALREV2)
tot$TOTALEXP2 <- gsub('\\,','',tot$TOTALEXP2)
tot$TOTALDEBT2 <- gsub('\\,','',tot$TOTALDEBT2)
tot = tot[,.(ENTITY_ID,TOTALREV2,TOTALEXP2,TOTALDEBT2,YEAR)]
setkey(tot,ENTITY_ID,YEAR)

tot = tot[!duplicated(tot),]
vs = c('TOTALREV2','TOTALEXP2','TOTALDEBT2')
tot = tot[,lapply(.SD,function(x) sum(as.numeric(x),na.rm=T)),by = .(ENTITY_ID,YEAR),.SDcols = vs]
setkey(tot,ENTITY_ID,YEAR)
setkey(spend_cast,ENTITY_ID,YEAR)
tot = tot[ENTITY_ID %in% spend_cast$ENTITY_ID,]
tot$ENTITY_NAME = spend_cast$ENTITY_NAME[match(tot$ENTITY_ID,spend_cast$ENTITY_ID)]
setkey(tot,ENTITY_ID,ENTITY_NAME,YEAR)
setkey(spend_cast,ENTITY_ID,ENTITY_NAME,YEAR)

alldt = spend_cast[tot,]

fdf = fread('input/florida_sd_data/all_district_info.csv')
fdf$DISTRICT_NAME = toupper(fdf$District_Name)
setnames(fdf, "County(ies)"   ,'County')
fdf = fdf[grepl('COMMUNITY DEVELOPMENT|INFRASTRUCTURE DEVELOPMENT',toupper(fdf$`Function(s)`)),]
dissolved = fread('input/florida_sd_data/fl_sd_dissolved.csv')
dissolved = dissolved[grepl('Community Development District',District_Name),]
fdf = merge(fdf,dissolved,all=T)
newname = fread('input/florida_sd_data/fl_sd_name_change.csv')
fdf$Old_Name = newname$Old_Name[match(fdf$District_Name,newname$New_Name)]
fdf$`Active or Inactive`[is.na(fdf$`Active or Inactive`)] <- 'Inactive'
fdf$District_Name = toupper(fdf$District_Name)
fdf$District_Name = gsub('\\s\\(NEW\\)$','',fdf$District_Name)
fdf$District_Name[fdf$District_Name=='TAPESTRY COMMUNITY DEVELOPMENT DISTRICT'&fdf$`Active or Inactive`=='Inactive'] <- 'TAPESTRY COMMUNITY DEVELOPMENT DISTRICT (OLD)'
fdf$District_Name = gsub("`","'",fdf$District_Name)

fdf$ENTITY_NAME = fdf$District_Name
fdf$ENTITY_NAME = gsub('COMMUNITY DEVELOPMENT DISTRICT','CDD',fdf$ENTITY_NAME)
fdf[,District_Name:=NULL]
fdf[,DISTRICT_NAME:=NULL]
fdf$ENTITY_NAME[fdf$ENTITY_NAME == 'Lake Beluthahatchee CDD - Dissolved ']<-  'Lake Beluthahatchee CDD'

alldt$ENTITY_NAME = toupper(alldt$ENTITY_NAME)
setkey(fdf,ENTITY_NAME)
setkey(alldt,ENTITY_NAME)

#alldt = fdf[alldt,]

library(sf)
library(tigris)
fdt = merge(fdf,alldt,all = T)

fnames = grep('EXP|REV',colnames(fdt),value=T)
#fdt[,(fnames):=lapply(.SD,zoo::na.fill,fill = 0),.SDcols = fnames]
fdt$MATCHED_DATA = (!is.na(fdt$County)) + 0
fdt = fdt[order(-MATCHED_DATA),]

fdt$Gov_Type = ifelse(grepl('^2',fdt$ENTITY_ID),'City','SD')
fdt = fdt[grepl('^2',fdt$ENTITY_ID)|grepl("CDD",fdt$ENTITY_NAME),]
fdt$Age_In_Years = fdt$YEAR - year(mdy(fdt$`Date Created/Established`))
fwrite(fdt,'input/florida_sd_data/cdd_panel_1993-2018.csv')

rev_donations = rev[grepl("Donation|Contribut",ACCOUNT_DESCRIPTION)][,sum(TOTAL_REVENUE),by = .(ENTITY_ID,ENTITY_NAME,YEAR)][grepl('CDD',ENTITY_NAME)]
setnames(rev_donations,'V1','REVENUE Donations/Contributions')
rev_impactfees = rev[grepl("Impact",ACCOUNT_DESCRIPTION)][,sum(TOTAL_REVENUE),by = .(ENTITY_ID,ENTITY_NAME,YEAR)][grepl('CDD',ENTITY_NAME)]
setnames(rev_impactfees,'V1','REVENUE Impact Fees')
rev_specialassessment = rev[grepl("Special Assessment",ACCOUNT_DESCRIPTION)][,sum(TOTAL_REVENUE),by = .(ENTITY_ID,ENTITY_NAME,YEAR)][grepl('CDD',ENTITY_NAME)]
setnames(rev_specialassessment,'V1','REVENUE Special Assessments')
rev_propertytax = rev[grepl("Ad Valorem",ACCOUNT_DESCRIPTION)][,sum(TOTAL_REVENUE),by = .(ENTITY_ID,ENTITY_NAME,YEAR)][grepl('CDD',ENTITY_NAME)]
setnames(rev_propertytax,'V1','REVENUE Property Tax')

rev_sources_tax_fee_assessment = merge(merge(merge(rev_impactfees,rev_specialassessment,by = c('ENTITY_ID','YEAR','ENTITY_NAME'),all=T),rev_propertytax,by = c('ENTITY_ID','YEAR','ENTITY_NAME'),all = T),rev_donations,by = c('ENTITY_ID','YEAR','ENTITY_NAME'),all=T)
rev_sources_tax_fee_assessment$TOTALREV2 = fdt$TOTALREV2[match(paste(rev_sources_tax_fee_assessment$ENTITY_ID,rev_sources_tax_fee_assessment$YEAR),paste(fdt$ENTITY_ID,fdt$YEAR))]
rev_sources_tax_fee_assessment$Age_In_Years = fdt$Age_In_Years[match(paste(rev_sources_tax_fee_assessment$ENTITY_ID,rev_sources_tax_fee_assessment$YEAR),paste(fdt$ENTITY_ID,fdt$YEAR))]

library(lubridate)
fwrite(rev_sources_tax_fee_assessment,'cdd_rev_sources_tax_fee_assessment.csv')


rev_source_melt = melt(rev_sources_tax_fee_assessment,id.vars = c('ENTITY_NAME','ENTITY_ID','YEAR','Age_In_Years','TOTALREV2'),variable.name = 'rev_type',value.name = 'rev_amount')
rev_source_melt$rev_amount[is.na(rev_source_melt$rev_amount)]<-0
rev_source_melt$Prop_Rev = rev_source_melt$rev_amount/rev_source_melt$TOTALREV2
rev_source_melt = rev_source_melt[Prop_Rev!=Inf]
temp = rev_source_melt[,mean(Prop_Rev,na.rm=T),by = .(Age_In_Years,rev_type)][Age_In_Years>=1]
ggplot(data = temp) + geom_line(aes(x = Age_In_Years,y = V1,col = rev_type)) + 
  scale_y_continuous(limits=c(0,1)) + theme_bw() + ggtitle('Prop. revenue, average by CDD age')

library(tidyverse)

rev_sources_tax_fee_assessment[`REVENUE Property Tax`==0]


ggplot(data =rev_sources_tax_fee_assessment) + 
  geom_boxplot(aes(x = as.factor(YEAR),y = `REVENUE Property Tax`/TOTALREV2))


  geom_boxplot(aes(x = as.factor(YEAR),y = `REVENUE Impact Fees`/TOTALREV2))

rev_sources_tax_fee_assessment
ggplot(rev_sources_tax_fee_assessment,aes(x = )) + factw_wa


do.call(merge,list(rev_buildingpermits,rev_impactfees,rev_specialassessment),by = c('ENTITY_ID','YEAR','ENTITY_NAME'))




rev_propertytax


rev[ACCOUNT_CD==321000]



