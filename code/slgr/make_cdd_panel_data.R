library(data.table)
require(tidyverse)
require(ggthemes)
library(readxl)

if(!require(ggradar)){devtools::install_github('ricardo-bion/ggradar');require(ggradar)}
#gaz = fread('input/uscb_gazetteer/2018_Gaz_place_national.txt')
#gaz = gaz[USPS=='FL',]

rev = fread('input/florida_lgfr/florida_lg_revenue_1993-2018.csv')
rev = setnames(rev,'DESCRIPTION','ACCOUNT_DESCRIPTION')
# city IDs start with 2, SD IDs start with 3, counties start with 1, other with 5
rev = rev[grepl('^(2|3)',rev$ENTITY_ID),]
#rev = rev[grepl('CDD|COMMUNITY DEVELOPMENT DIST',toupper(ENTITY_NAME)),]

repcols =  colnames(rev[,GENERAL:COMPONENT_UNITS])
rev[,(repcols):=lapply(.SD,function(x) as.numeric(gsub("\\,",'',x))),.SDcols = repcols]
rev$TOTAL_REVENUE = rowSums(rev[,GENERAL:COMPONENT_UNITS],na.rm=T)
rev[,Year:=NULL]
rev$Gov_Type = ifelse(grepl('^2',rev$ENTITY_ID),'City','SD')
#rev[Gov_Type == 'City'|grepl('CDD|Community Development District',ENTITY_NAME),]

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
library(forcats)
rev_comb$GEN_CAT = as.factor(rev_comb$GEN_CAT_DESC)
rev_comb$GEN_CAT = fct_other(rev_comb$GEN_CAT,drop = c("Sales","Interest and Other Earnings","Other Sources-Transfers" ,"Judgments, Fines and Forfeits" ,"Court-Related Revenues" , "Miscellaneous Revenues"),other_level = 'Other Sources')
rev_comb$GEN_CAT = fct_other(rev_comb$GEN_CAT, drop = c("State Grants","Federal Grants" ,"State Shared","Federal Payments in Lieu of Taxes", "State Payments in Lieu of Taxes" ),other_level = 'State and Federal Grants and Payments')
rev_comb$GEN_CAT = fct_other(rev_comb$GEN_CAT, drop = c("Local Payments in Lieu of Taxes","Local Government Unit Grants","Shared Local Government Unit Grants" ),other_level = 'Local Grants and Payments')
rev_comb$GEN_CAT = as.character(rev_comb$GEN_CAT)
rev_comb$GEN_CAT[is.na(rev_comb$GEN_CAT)&rev_comb$SPEC_CAT_DESC == 'Impact Fees'] <- 'Permits, Fees and Licenses'
rev_comb$GEN_CAT[is.na(rev_comb$GEN_CAT)&rev_comb$SPEC_CAT_DESC == 'Special Assessments'] <- 'Special Assessments'
rev_comb$GEN_CAT[rev_comb$SPEC_CAT_DESC == 'Special Assessments'] <- 'Special Assessments'
rev_comb$GEN_CAT[is.na(rev_comb$GEN_CAT)] <- 'Other Sources'

rev_comb[grepl('^3',ENTITY_ID)&GEN_CAT=='Other Sources',][,.N,by=.(ACCOUNT_DESCRIPTION)][order(-N)]

rev_tot = rev_comb[,sum(TOTAL_REVENUE),by = .(ENTITY_ID,ENTITY_NAME,YEAR,Gov_Type,GEN_CAT)]
setnames(rev_tot,'V1','REVENUE')
rev_tot$GEN_CAT = paste0('REVENUE ',rev_tot$GEN_CAT)
rev_cast = dcast(rev_tot,ENTITY_ID + ENTITY_NAME + YEAR + Gov_Type ~ GEN_CAT,value.var = 'REVENUE',fill  = 0)

exp = fread('input/florida_lgfr/florida_lg_expenditure_1993-2018.csv')
exp = exp[grepl('^(2|3)',exp$ENTITY_ID),]
exp = setnames(exp,'DESCRIPTION','ACCOUNT_DESCRIPTION')
exp[,Year:=NULL]
#exp = exp[grepl('CDD|COMMUNITY DEVELOPMENT DIST',toupper(ENTITY_NAME)),]
repcols =  colnames(exp[,GENERAL:COMPONENT_UNITS])
exp[,(repcols):=lapply(.SD,function(x) as.numeric(gsub("\\,",'',x))),.SDcols = repcols]
exp$TOTAL_EXPENDITURE = rowSums(exp[,GENERAL:COMPONENT_UNITS],na.rm=T)
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

library(tidyverse)
library(data.table)
library(lubridate)
fdf = fread('input/florida_sd_data/all_district_info.csv')
fdf$DISTRICT_NAME = toupper(fdf$District_Name)
setnames(fdf, "County(ies)"   ,'County')
fdf = fdf[grepl('COMMUNITY DEVELOPMENT|INFRASTRUCTURE DEVELOPMENT',toupper(fdf$`Function(s)`)),]
dissolved = fread('input/florida_sd_data/fl_sd_dissolved.csv')
dissolved = dissolved[grepl('Community Development District',District_Name),]
dissolved[,County:=NULL]
dissolved$Dissolution_Date = mdy(dissolved$Date_Dissolved)
dissolved = dissolved[grepl('CDD|Community Development District',District_Name),]

# gg_fig1 = ggplot(dissolved,aes(x = year(Dissolution_Date))) + geom_bar() + theme_bw() + 
#   ggtitle('# CDDs Dissolved by year') + scale_x_continuous(name = 'Year',breaks = seq(1993,2019,3)) + 
#   scale_y_continuous('# dissolved',limits = c(0,15),expand=c(0,0)) + theme(text= element_text(family = 'Times',size=12))

fdf = merge(fdf,dissolved,all=T)
newname = fread('input/florida_sd_data/fl_sd_name_change.csv')
newname[,County:=NULL]
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

#alldt = alldt[Gov_Type=='SD']
alldt$ENTITY_NAME = toupper(alldt$ENTITY_NAME)
fdf$ENTITY_NAME = toupper(fdf$ENTITY_NAME)
alldt$ENTITY_NAME = gsub('#','',alldt$ENTITY_NAME,fixed=T)
alldt$ENTITY_NAME = gsub('\\sNO(\\.|)\\s',' ',alldt$ENTITY_NAME)
fdf$ENTITY_NAME = gsub('\\sNO(\\.|)\\s',' ',fdf$ENTITY_NAME)
fdf$ENTITY_NAME = gsub('(\\s|)\\(.*\\)','',fdf$ENTITY_NAME)
alldt$ENTITY_NAME = gsub('(\\s|)\\(.*\\)','',alldt$ENTITY_NAME)
fdf$ENTITY_NAME = gsub('COMMUNITY DISTRICT','CDD',fdf$ENTITY_NAME)
fdf$ENTITY_NAME = gsub('\\, THE$','',fdf$ENTITY_NAME)
alldt$ENTITY_NAME = gsub('\\, THE$','',alldt$ENTITY_NAME)
fdf$ENTITY_NAME = gsub('MAIN STREET','MAINSTREET',fdf$ENTITY_NAME)
alldt$ENTITY_NAME = gsub('MAIN STREET','MAINSTREET',alldt$ENTITY_NAME)
fdf$ENTITY_NAME = gsub('MAIN STREET','MAINSTREET',fdf$ENTITY_NAME)
alldt$ENTITY_NAME = gsub('MAIN STREET','MAINSTREET',alldt$ENTITY_NAME)
fdf$ENTITY_NAME = gsub('\\s(I|1)$','',fdf$ENTITY_NAME)
alldt$ENTITY_NAME = gsub('\\s(I|1)$','',alldt$ENTITY_NAME)
fdf$ENTITY_NAME = gsub('-',' ',fdf$ENTITY_NAME)
alldt$ENTITY_NAME = gsub('-',' ',alldt$ENTITY_NAME)

fdf$ENTITY_NAME = gsub('\\.','',fdf$ENTITY_NAME)
alldt$ENTITY_NAME = gsub('\\.','',alldt$ENTITY_NAME)

fdf$ENTITY_NAME = gsub('\\s\\/\\s',' ',fdf$ENTITY_NAME)
alldt$ENTITY_NAME = gsub('\\s\\/\\s',' ',alldt$ENTITY_NAME)
fdf$ENTITY_NAME = gsub('\\/',' ',fdf$ENTITY_NAME)
alldt$ENTITY_NAME = gsub('\\/',' ',alldt$ENTITY_NAME)
alldt$ENTITY_NAME = gsub("('|`)",'',alldt$ENTITY_NAME)
fdf$ENTITY_NAME = gsub("('|`)",'',fdf$ENTITY_NAME)
fdf$ENTITY_NAME = gsub( "MT DORA","MOUNT DORA",fdf$ENTITY_NAME)
alldt$ENTITY_NAME = gsub( "MT DORA","MOUNT DORA",alldt$ENTITY_NAME)
fdf$ENTITY_NAME = gsub( "WILDWOOD CDD","VILLAGE CDD 12",fdf$ENTITY_NAME)
alldt$ENTITY_NAME = gsub( "WILDWOOD CDD","VILLAGE CDD 12",alldt$ENTITY_NAME)
fdf$ENTITY_NAME = gsub( "ISLANDS AT DORAL$","ISLANDS AT DORAL CDD",fdf$ENTITY_NAME)
alldt$ENTITY_NAME = gsub( "ISLANDS AT DORAL$","ISLANDS AT DORAL CDD",alldt$ENTITY_NAME)
fdf$ENTITY_NAME = gsub( "CHAMPIONSGATE","CHAMPIONS GATE",fdf$ENTITY_NAME)
alldt$ENTITY_NAME = gsub( "CHAMPIONSGATE","CHAMPIONS GATE",alldt$ENTITY_NAME)
fdf$ENTITY_NAME = gsub( "TAMPA PALMS OPEN SPACE & TRANSPORTATION CDD","TAMPA PALMS OPEN SPACE AND TRANSPORTATION CDD",fdf$ENTITY_NAME)
alldt$ENTITY_NAME = gsub( "TAMPA PALMS OPEN SPACE & TRANSPORTATION CDD","TAMPA PALMS OPEN SPACE AND TRANSPORTATION CDD",alldt$ENTITY_NAME)

fdf$ENTITY_NAME = gsub( "XENTRY","XENTURY",fdf$ENTITY_NAME)
alldt$ENTITY_NAME = gsub( "XENTRY","XENTURY",alldt$ENTITY_NAME)

fdf$ENTITY_NAME = gsub( "CDD2","CDD 2",fdf$ENTITY_NAME)
alldt$ENTITY_NAME = gsub( "CDD2","CDD 2",alldt$ENTITY_NAME)
fdf$ENTITY_NAME = gsub( "CDD3","CDD 3",fdf$ENTITY_NAME)
alldt$ENTITY_NAME = gsub( "CDD3","CDD 3",alldt$ENTITY_NAME)
alldt$ENTITY_NAME = gsub('\\s{2,}',' ',alldt$ENTITY_NAME)
fdf$ENTITY_NAME = gsub('\\s{2,}',' ',fdf$ENTITY_NAME)

fdf$CREATED_BY<-NA
fdf$CREATED_BY[fdf$`Creation Method` =='Local Ordinance'&grepl('County',fdf$`Local Governing Authority`)] <- 'County'
fdf$CREATED_BY[fdf$`Creation Method` =='Local Ordinance'&grepl('Town|City',fdf$`Local Governing Authority`)] <- 'City'
fdf$CREATED_BY[is.na(fdf$CREATED_BY)]<-'State/General Law'


alldt = alldt[!(TOTALREV2==0&TOTALEXP2==0&TOTALDEBT2==0),]
fwrite(alldt[Gov_Type=='City'],'input/florida_sd_data/city_panel_1993-2018.csv')
setkey(fdf,ENTITY_NAME)
setkey(alldt,ENTITY_NAME)
fdt = data.table(inner_join(fdf,alldt))
fdt$Year_Created = year(mdy(fdt$`Date Created/Established`))
fdt$Age = fdt$YEAR-fdt$Year_Created
fwrite(alldt[Gov_Type=='SD'],'input/florida_sd_data/cdd_panel_1993-2018.csv')

tt = fdt[YEAR==2018,][,list(.N),by=.(CREATED_BY)]
tt$prop = tt$N/sum(tt$N)

fdt2018 = fdt[YEAR==2018,][TOTALREV2>0,][is.na(Dissolution_Date),]
fdt_2018_long = melt(fdt2018[,.(CREATED_BY,TOTALREV2,TOTALEXP2,TOTALDEBT2)])


ggplot(fdt_2018_long) + 
  geom_density(aes(x = value,col = CREATED_BY),trim = T) + 
  facet_wrap(~variable,ncol = 1,scales = 'free') + theme_bw()

1e6/1000000
require(htmlTable)

tab = do.call(rbind,list(round(rbind(do.call(rbind,tapply(fdt2018$TOTALREV2,fdt2018$CREATED_BY,summary)),
      summary(fdt2018$TOTALREV2))/1e6,3),
round(rbind(do.call(rbind,tapply(fdt2018$TOTALEXP2,fdt2018$CREATED_BY,summary)),
            summary(fdt2018$TOTALEXP2))/1e6,3),
round(rbind(do.call(rbind,tapply(fdt2018$TOTALDEBT2,fdt2018$CREATED_BY,summary)),
            summary(fdt2018$TOTALDEBT2))/1e6,3)))

rownames(tab)[rownames(tab)==''] <- '(any)'
htmlTable(tab[,c(1,3,4,6)],
          rgroup = c('Total Revenue ($1M)','Total Expenditure ($1M)', 'Debt Outstanding ($1M)'),
          n.rgroup = rep(4,3))

tapply(fdt2018$TOTALEXP2,fdt2018$CREATED_BY,summary)
tapply(fdt2018$TOTALDEBT2,fdt2018$CREATED_BY,summary)


summary(fdt2018[,.(TOTALREV2,TOTALEXP2,TOTALDEBT2)])

require(htmlTable)





as.table(tapply(fdt2018$TOTALEXP,fdt2018$CREATED_BY,summary))



fdt2018


tapply(fdt_2018_long$value,fdt_2018_long$var
fdt_2018_long[value==0,]

ggplot(fdt_2018_long[variable == 'TOTALDEBT2']) + geom_boxplot(aes(x = variable,y = value,col = CREATED_BY)) +
  scale_y_continuous(trans = scales::pseudo_log_trans(base = 2))


fdt2018[,lapply(.SD,mean),.SDcols = c('TOTALREV2','TOTALEXP2','TOTALDEBT2'),by=.(CREATED_BY)]


ggplot(fdt) + geom_bar(aes(x=YEAR,fill = CREATED_BY))
ggplot() + geom_bar(aes(x=YEAR,fill = CREATED_BY),)

fdt$TOTALDEBT2
fdt$TOTALEXP2
fdt$TOTALREV2


library(data.table)
rep_by_year = fdt[,.N,by = .(YEAR)]
rep_by_year$YEAR <- as.numeric(rep_by_year$YEAR)
gg_fig1 = ggplot(rep_by_year[order(YEAR),],aes(x = YEAR,y = N)) + geom_path() + geom_point() + theme_bw() + ylab("Total reports filed")+
  ggtitle('# of CDD financial reports by year')  + scale_x_continuous(name = 'Year',breaks = seq(1993,2019,3)) + 
  theme(text = element_text(family = 'Times',size = 12))

ggsave(plot = gg_fig1,filename = 'output/slgr/slgr_figure1.png',dpi = 500,width = 6,height = 5,units = 'in')


utility_service_tax_codes = c(314100,314300, 314400, 314700	, 314800,314900	)
utility_franchise_fee_codes = c(323100, 323200,    323300	,  323400	, 323500	,  323600	,   323700	, 323900	)
utility_federal_grant_codes = c(331310, 331320	, 331330	,331340	,  331350	)
utility_state_grant_and_revshare_codes = c(334310, 334320,  334330	, 334340	, 334350,
                                           335310	,   335320	,   335330	,  335340	,335350	)
utility_service_charge_codes = c(343100,   343200	,  343300	, 343400	, 343500	,  343600	)

util_vals = as.character(as.vector(unlist(sapply(grep('^utility.*codes$',ls(),value=T),get))))
util_rev = rev_comb[as.character(ACCOUNT_CD) %in% util_vals & ENTITY_ID %in% fdt$ENTITY_ID,]
util_rev$SOURCE = NA
util_rev$SOURCE[util_rev$ACCOUNT_CD %in% utility_service_tax_codes] <- 'Service Tax'
util_rev$SOURCE[util_rev$ACCOUNT_CD %in% utility_service_charge_codes] <- 'Service Charge'
util_rev$SOURCE[util_rev$ACCOUNT_CD %in% utility_federal_grant_codes] <- 'Federal Grant'
util_rev$SOURCE[util_rev$ACCOUNT_CD %in% utility_state_grant_and_revshare_codes] <- 'State Grant/Rev. Share'
util_rev$SOURCE[util_rev$ACCOUNT_CD %in% utility_franchise_fee_codes] <- 'Franchise Fees'
util_rev_sum = util_rev[,sum(TOTAL_REVENUE),by = .(ENTITY_ID,ENTITY_NAME,YEAR,SOURCE)]
util_rev_sum$SOURCE = paste('Utility',util_rev_sum$SOURCE)
utility_revenue_sources = dcast(util_rev_sum,ENTITY_ID + YEAR ~ SOURCE,value.var = 'V1',fill = 0)
fwrite(utility_revenue_by_source_type,'input/florida_lgfr/cdd_utility_revenue_by_source_type.csv')


exp_general  = exp[EXP_FUNCTION_DESC=='General Government' & ENTITY_ID %in% alldt$ENTITY_ID,]
exp_general = exp_general[,.(ENTITY_ID,ENTITY_NAME,YEAR,TOTAL_EXPENDITURE,OBJECT_CD_DESC)] 
exp_general = exp_general[,sum(TOTAL_EXPENDITURE),by = .(ENTITY_ID,ENTITY_NAME,YEAR,OBJECT_CD_DESC)]
general_government_expenditures_by_object = dcast(exp_general , ENTITY_ID + ENTITY_NAME + YEAR ~ OBJECT_CD_DESC,value.var = 'V1',fill = 0)
fwrite(general_government_expenditures_by_object ,'input/general_government_expenditures_by_object.csv')

exp_general  = exp[EXP_FUNCTION_DESC=='General Government'& ENTITY_ID %in% alldt$ENTITY_ID,,]
exp_general = exp_general[,.(ENTITY_ID,ENTITY_NAME,YEAR,TOTAL_EXPENDITURE,ACCOUNT_DESCRIPTION)] 
exp_general = exp_general[,sum(TOTAL_EXPENDITURE),by = .(ENTITY_ID,ENTITY_NAME,YEAR,ACCOUNT_DESCRIPTION)]
general_government_expenditures_by_function = dcast(exp_general , ENTITY_ID + ENTITY_NAME + YEAR ~ ACCOUNT_DESCRIPTION,value.var = 'V1',fill = 0)
fwrite(general_government_expenditures_by_function ,'general_government_expenditures_by_function.csv')

gen_sub = general_government_expenditures_by_function[ENTITY_ID %in% fdt$ENTITY_ID]
gen_sub$Total_General_Exp = rowSums(gen_sub[,`Comprehensive Planning`:`Financial and Administrative`])
gen_sub = cbind(gen_sub[,.(ENTITY_ID,YEAR)],gen_sub[,`Comprehensive Planning`:`Financial and Administrative`] / gen_sub$Total_General_Exp)
gen_sub = cbind(fdt,gen_sub[match(paste(fdt$ENTITY_ID,fdt$YEAR),paste(gen_sub$ENTITY_ID,gen_sub$YEAR)),`Comprehensive Planning`:`Financial and Administrative`])
gen_sub = melt(gen_sub,id.vars = c('ENTITY_ID','Age'),measure.vars = c("Comprehensive Planning","Financial and Administrative","Debt Service Payments","Executive"))
temp = gen_sub[,list(mean(value,na.rm=T)),by = .(Age,variable)][Age>=1]

(gg_fig6 =ggplot(data =temp) + geom_path(aes(x = Age,y = V1,col = variable),lwd = 1) +
  #geom_point(aes(x = Age,y = V2,col = rev_type,size = N^(1/3)),alpha = 0.5) +
  scale_y_continuous(limits=c(0,1),name = 'prop. gen. gov. expenditure') + theme_bw() + 
  scale_x_continuous(expand=c(0,0)) +
  ggtitle('Prop. general gov. expenditure by age') + 
  scale_color_tableau(name = 'Expenditure') + 
                    #  labels = c('Comprehensive planning','Financial/admin.','Debt service','Executive')) + 
  theme(legend.position = c(0.25,0.25),legend.background = element_rect(fill = alpha('white',0.5))) + 
  ggtitle('Average proportion of general gov. expenditure by CDD age') + 
  theme(text = element_text(family = 'Times',size = 12)) )

ggsave(plot = gg_fig6,filename = 'output/slgr/slgr_figure6.png',dpi = 500,width = 6,height = 5,units = 'in')


gen_sub = general_government_expenditures_by_function[ENTITY_ID %in% alldt$ENTITY_ID]
gen_sub$Total_General_Exp = rowSums(gen_sub[,`Comprehensive Planning`:`Financial and Administrative`])
gen_sub = cbind(gen_sub[,.(ENTITY_ID,YEAR)],gen_sub[,`Comprehensive Planning`:`Financial and Administrative`] / gen_sub$Total_General_Exp)
gen_sub$CDD = grepl('^3',gen_sub$ENTITY_ID)
#dropping these NAs gets rid of cases where entitie filed total exp data but no itemized data
gen_sub= gen_sub[!is.na(Executive)]
gen_sub = melt(gen_sub,id.vars = c('ENTITY_ID','CDD','YEAR'))
gen_sub = gen_sub[,list(.N,mean(value,na.rm=T)),by = .(variable,CDD)]
temp_cast3=dcast(gen_sub,CDD ~ variable,value.var = 'V2')
colnames(temp_cast3) <- c('CDD','Comp.\nPlanning','Debt\nService','Executive','Financial/\nAdmin.')

(gg_fig3C = ggradar(temp_cast3,font.radar = 'Times',  group.point.size = 2.5,
                  values.radar = c("0%", "35%", "75%"),axis.label.size = 2.5,
                  grid.min = 0, # 10,
                  grid.mid = 0.35, # 50,
                  grid.max = 0.75) + 
  scale_color_economist(labels = c('City','CDD')) +
    theme(text = element_text(size = 12),legend.background = element_rect(fill = NA),plot.margin = unit(c(.1,.1,.1,.1), "cm")) + 
  ggtitle('C: % of general expenditure by target'))


gen_sub = general_government_expenditures_by_function[ENTITY_ID %in% alldt$ENTITY_ID]
gen_sub$Total_General_Exp = rowSums(gen_sub[,`Comprehensive Planning`:`Financial and Administrative`])
gen_sub$TOTALEXP2 = alldt$TOTALEXP2[match(paste(gen_sub$ENTITY_ID,gen_sub$YEAR),paste(alldt$ENTITY_ID,alldt$YEAR))]
gen_sub = cbind(gen_sub[,.(ENTITY_ID,YEAR)],gen_sub[,`Comprehensive Planning`:`Financial and Administrative`] / gen_sub$TOTALEXP2)
gen_sub$CDD = grepl('^3',gen_sub$ENTITY_ID)
#dropping these NAs gets rid of cases where entitie filed total exp data but no itemized data
gen_sub= gen_sub[!is.na(Executive)]
gen_sub = melt(gen_sub,id.vars = c('ENTITY_ID','CDD','YEAR'))
gen_sub = gen_sub[,list(.N,mean(value,na.rm=T)),by = .(variable,CDD)]
temp_cast=dcast(gen_sub,CDD ~ variable,value.var = 'V2')
colnames(temp_cast) <- c('CDD','Comp. planning','Debt service','Executive','Financial/admin.')
# 
# (gg_fig3D = ggradar(temp_cast,font.radar = 'Times',  group.point.size = 2.5,
#                     values.radar = c("0%", "20%", "40%"),
#                     grid.min = 0, # 10,
#                     grid.mid = 0.20, # 50,
#                     grid.max = 0.40) + 
#   scale_color_economist(labels = c('City','CDD')) +theme(legend.position = c(0.9,0.05),legend.background = element_rect(fill = NA)) + 
#   ggtitle('General gov. expenditures by cities and CDDs',sub = '% of total expenditure'))


temp = cbind(fdt[,.(ENTITY_ID,YEAR,TOTALREV2,Age)],fdt[,`REVENUE Ad Valorem Taxes`:`REVENUE State and Federal Grants and Payments`])
#temp$`REVENUE Gov. Grants and Payments` = temp$`REVENUE Local Grants and Payments` + temp$`REVENUE State and Federal Grants and Payments`
temp[,`REVENUE Other Sources`:= `REVENUE Other Sources` + `REVENUE Local Grants and Payments` +  `REVENUE Rents and Royalties` + `REVENUE State and Federal Grants and Payments` + `REVENUE General Government Taxes`]
temp[,`REVENUE Local Grants and Payments`:=NULL]
temp[,`REVENUE State and Federal Grants and Payments`:=NULL]
temp[,`REVENUE Rents and Royalties`:=NULL]
temp[,`REVENUE General Government Taxes`:=NULL]

temp = melt(temp,id.vars = c('ENTITY_ID',"YEAR",'TOTALREV2','Age'),value.name = 'rev',variable.name = 'rev_type')
temp$Prop_Rev = temp$rev/temp$TOTALREV2
temp = temp[!is.na(Prop_Rev)]
temp1 = temp[,list(.N,mean(Prop_Rev,na.rm=T)),by = .(Age,rev_type)][Age>=1]
library(ggthemes)

temp2 = temp[!duplicated(paste(ENTITY_ID,YEAR)),.(TOTALREV2,Age)][,mean(TOTALREV2,na.rm=T),by=.(Age)][Age>=1]

(g4A = ggplot(data =temp1) + geom_path(aes(x = Age,y = V2,col = rev_type),lwd = 1) +
  #geom_point(aes(x = Age,y = V2,col = rev_type,size = N^(1/3)),alpha = 0.5) +
  scale_y_continuous(limits=c(0,1),name = 'prop. total revenue') + theme_bw() + 
  scale_x_continuous(expand=c(0,0))+
  ggtitle('Prop. revenue, average by CDD age') + 
  scale_color_tableau(name = 'Revenue source',labels = c('Property taxes','Contributions/donations','Other sources','Permits/fees','Service charges','Special assessments')) + 
                        # labels = c('Property taxes','Contributions/donations','Service charges','Permits/fees')) + 
  theme(legend.position = c(0.5,0.45),legend.background = element_rect(fill = alpha('white',0.5))) + 
  ggtitle('Average proportion of total revenue by CDD age') + 
  theme(text = element_text(family = 'Times',size = 12),axis.title.x = element_blank()) )
# (g4B =  ggplot(data =temp1) + geom_ribbon(aes(ymin = 0,ymax = N,x = Age)) + theme_bw() + scale_x_continuous(expand= c(0,0)) +
#   scale_y_continuous(name = '# CDDs observed',breaks = c(0,150,300,450)) + ggtitle('# CDDs observed by age'))
# 
# grid.arrange(g4A,g4B,ncol = 1,heights = c(0.8,0.2))

ggsave(plot = g4A,filename = 'output/slgr/slgr_figure4.png',dpi = 500,width = 6,height = 5,units = 'in')



library(gridExtra)

temp = fdt[,.(ENTITY_ID,YEAR,`EXPENDITURE General Government`,`EXPENDITURE Transportation`,`EXPENDITURE Public Safety`,`EXPENDITURE Physical Environment`,TOTALEXP2,Age)]
temp = melt(temp,id.vars = c('ENTITY_ID',"YEAR",'TOTALEXP2','Age'),value.name = 'exp',variable.name = 'exp_type')
temp$Prop_Exp = temp$exp/temp$TOTALEXP2
temp = temp[!is.na(Prop_Exp)]
temp = temp[,list(.N,mean(Prop_Exp,na.rm=T)),by = .(Age,exp_type)][Age>=1]
gg_fig5= ggplot(data =temp) + geom_path(aes(x = Age,y = V2,col = exp_type),lwd = 1) +
  #geom_point(aes(x = Age,y = V2,col = rev_type,size = N^(1/3)),alpha = 0.5) +
  scale_y_continuous(limits=c(0,1),name = 'prop. total expenditure') + theme_bw() + 
  scale_x_continuous(expand=c(0,0))+
  ggtitle('Prop. expend., average by CDD age') + 
  scale_color_tableau(palette = 'Color Blind',name = 'Object',labels = c('General gov.','Transportation','Public safety','Public works')) + 
  #     labels = c('Property taxes','Contributions/donations','Service charges','Assessments/permits/fees')) + 
  theme(legend.position = c(0.2,0.3),legend.background = element_rect(fill = alpha('white',0.5))) + 
  ggtitle('Average proportion of total expenditure by CDD age') + 
  theme(text = element_text(family = 'Times',size = 12),axis.title.x = element_blank()) 
gg_fig5

ggsave(plot = gg_fig5,filename = 'output/slgr/slgr_figure5.png',dpi = 500,width = 6,height = 5,units = 'in')


temp = fdt[,list(sum(TOTALREV2),sum(TOTALDEBT2)),by = .(YEAR)][order(YEAR)]

bondissues = data.table(read_excel('input/CommDeveDistrictsAll10082019.xlsx'))
bondissues$Year = year(ymd(bondissues$`Dated Date`))
bondissues$Year[bondissues$Year==2102] <- 2012
bonds_by_year = bondissues[,sum(`Amount Issued`),by = .(Year)][order(Year)]
bonds_by_year = bonds_by_year[Year>=2006,]

gg_fig2 = ggplot() + 
  geom_path(data = temp,aes(x = YEAR,y = V2/1e9,col = 'Total debt'),lwd = 1)+
  geom_path(data = temp,aes(x = YEAR,y = V1/1e9,col = 'Total revenue'),lwd = 1)+
  geom_point(data = temp,aes(x = YEAR,y = V2/1e9,col = 'Total debt'))+
  geom_point(data = temp,aes(x = YEAR,y = V1/1e9,col = 'Total revenue')) + 
  geom_point(data = bonds_by_year,aes(x = Year,y = V1/1e9,col = 'Bonds issued'))+
  geom_path(data = bonds_by_year,aes(x = Year,y = V1/1e9,col = 'Bonds issued'),lwd = 1)+
  scale_x_continuous(name = 'Year') +
  scale_y_continuous(name = '$ billion') + theme_bw() + 
  theme(legend.position = c(0.2,0.8),legend.background = element_rect(fill = alpha('white',0.5)),
        legend.title = element_blank(),text = element_text(family = 'Times',size = 12)) + 
  scale_color_brewer(name = '',type = 'qual',palette = 1) +
  ggtitle('CDD total revenue and debt by year')

ggsave(plot = gg_fig2,filename = 'output/slgr/slgr_figure2.png',dpi = 500,width = 6,height = 5,units = 'in')


temp = alldt[,grep('ENTITY_ID|YEAR|REVENUE|TOTALREV2',colnames(alldt)),with=F]
temp$CDD = grepl('^3',temp$ENTITY_ID)
temp = melt(temp,id.vars = c('ENTITY_ID',"YEAR",'TOTALREV2','CDD'),value.name = 'rev',variable.name = 'rev_type')
temp$Prop_Rev = temp$rev/temp$TOTALREV2
temp = temp[!is.na(Prop_Rev)]
temp = temp[,list(.N,mean(Prop_Rev,na.rm=T)),by = .(rev_type,CDD)]

# Example from https://github.com/ricardo-bion/ggradar
library(ggplot2)
if(!require(ggradar)){devtools::install_github('https://github.com/ricardo-bion/ggradar');require(ggradar)}
library(scales)
temp$rev_type = gsub('REVENUE ','',temp$rev_type)
temp$rev_type[temp$rev_type=='Permits, Fees and Licenses'] <- c('Permits/fees')
temp$rev_type[temp$rev_type=='State and Federal Grants and Payments'] <- c('Grants')
temp$rev_type[temp$rev_type=='Ad Valorem Taxes'] <- c('Property\ntaxes')
temp$rev_type[temp$rev_type=='Rents and Royalties'] <- c('Other')
temp$rev_type[temp$rev_type=='Other Sources'] <- c('Other')
temp$rev_type[temp$rev_type=='Service Charges'] <- c('Service\ncharges')
temp$rev_type[temp$rev_type=='Local Grants and Payments'] <- c('Grants')
temp$rev_type[temp$rev_type=='General Government Taxes'] <- c('General\ntaxes')
temp$rev_type[temp$rev_type=='Contributions and Donations'] <- c('Private\ncontributions')
temp_cast1=dcast(temp,CDD ~ rev_type,value.var = 'V2',fun.aggregate = sum)

temp_cast1

gg_fig3A = ggradar(temp_cast1,font.radar = 'Times',  group.point.size = 3,axis.label.size = 2.5,
        values.radar = c("0%", "30%", "60%"),
        grid.min = 0, # 10,
        grid.mid = 0.3, # 50,
        grid.max = 0.6) + 
  scale_color_economist(labels = c('City','CDD')) +theme(text = element_text(size = 12),plot.margin = unit(c(.1,.1,.1,.1), "cm"),legend.background = element_rect(fill = NA)) + 
  ggtitle('A: % of total revenue by source') + guides(colour = F)


temp = alldt[,grep('ENTITY_ID|YEAR|EXPEND|TOTALEXP2',colnames(alldt)),with=F]
temp$CDD = grepl('^3',temp$ENTITY_ID)
temp = melt(temp,id.vars = c('ENTITY_ID',"YEAR",'TOTALEXP2','CDD'),value.name = 'exp',variable.name = 'exp_type')
temp$exp_type = gsub('EXPENDITURE ','',temp$exp_type)
temp$exp_type[temp$exp_type=='Physical Environment'] <- 'Public\nWorks'
temp$exp_type[temp$exp_type=='Public Safety'] <- 'Public\nSafety'
temp$exp_type[temp$exp_type=="Economic Environment"] <- "Economic\nEnvironment"
temp$exp_type[temp$exp_type=="Human Services"] <- "Human\nServices"
temp$exp_type[temp$exp_type=="General Government" ] <- "General\nGovernment" 

temp$Prop_Exp = temp$exp/temp$TOTALEXP2
#dropping these NAs gets rid of cases where entitie filed total exp data but no itemized data
temp = temp[!is.na(Prop_Exp)]
temp = temp[,list(.N,mean(Prop_Exp,na.rm=T)),by = .(exp_type,CDD)]
temp_cast2=dcast(temp,CDD ~ exp_type,value.var = 'V2')
temp_cast2[,Schools:=NULL]
gg_fig3B= ggradar(temp_cast2,font.radar = 'Times',  group.point.size = 2.5,
                  values.radar = c("0%", "35%", "75%"),axis.label.size = 2.5,
                  grid.min = 0, # 10,
                  grid.mid = 0.35, # 50,
                  grid.max = 0.75) + 
  scale_color_economist(labels = c('City','CDD')) +theme(plot.margin = unit(c(.1,.1,.1,.1), "cm"),text = element_text(size = 12),legend.background = element_rect(fill = NA)) + 
  ggtitle('B: % of total expenditure by target') 

require(gridExtra)





#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

commonlegend<-g_legend(gg_fig3B)

gg_fig3 <- grid.arrange(gg_fig3A+ theme(legend.position = 'none'),gg_fig3B + theme(legend.position = 'none'),gg_fig3C+ theme(legend.position = 'none'),commonlegend,ncol = 2,
                        heights = c(1,1),widths = c(1,1),padding = 0)

ggsave(plot = gg_fig3,filename = 'output/slgr/slgr_figure3.png',dpi = 1000,width = 7,height= 7,units = 'in')


require(tigris)
require(sf)
florida = tigris::counties(state = 'FL',cb = T,year = 2018)

fdt2018_county = fdt2018[,list(.N,sum(TOTALDEBT2)),by=.(County)]


fdt2018_county = fdt2018_county[sapply(str_split(fdt2018_county$County,', '),length)==1,]
#split 273041812 between Duval, St. Johns
fdt2018_county$V2[fdt2018_county$County%in%c('Duval','St. Johns')] = fdt2018_county$V2[fdt2018_county$County%in%c('Duval','St. Johns')] + 273041812 /2
fdt2018_county$N[fdt2018_county$County%in%c('Duval','St. Johns','Glades','Hendry')] = fdt2018_county$N[fdt2018_county$County%in%c('Duval','St. Johns','Glades','Hendry')] + 0.5

florida = cbind(florida,fdt2018_county[match(toupper(florida$NAME),toupper(fdt2018_county$County)),.(N,V2)])

devtools::install_github("yonghah/esri2sf")
library("esri2sf")

cbsa = tigris::core_based_statistical_areas(cb = T,year = '2018')
cbsa = cbsa[grepl('FL',cbsa$NAME),]

require(tidycensus)

cpops = fread('https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv')
cpops$FIPS = paste0(formatC(cpops$STATE,width = 2, flag = 0),formatC(cpops$COUNTY,width = 3,flag = 0))
cpops = cpops[STATE=='12',.(FIPS,POPESTIMATE2018)]
florida$pop2018 = cpops$POPESTIMATE2018[match(florida$GEOID,cpops$FIPS)]               
      


f1 = ggplot() + geom_sf(data = florida,aes(fill = pop2018/1e6)) + theme_map() +
  ggtitle("A: County population, 2018")+
  scale_fill_viridis_c(na.value = 'grey80',option = 'A',
                       direction = -1,name = 'Population (1M)')+ 
  theme(legend.position = c(0.2,0.25))

test = st_read('https://opendata.arcgis.com/datasets/7ad4473a939d473ab8c17f83155aedbe_0.geojson')
ggplot() + geom_sf(data = test)

require(ggrepel)
fl_csa= cbsa[order(-cbsa$ALAND),][1:8,]
fl_csa$NAME <- str_remove(fl_csa$NAME,'\\,.*')

fl_csa$NAME = str_remove(fl_csa$NAME,'-[A-Za-z\\s]+$')

fl_csa$NAME[grepl(fl_csa$NAME] <- 'Naples'

fl_places = tigris::places('FL',cb = T,year = '2018')



urban = tigris::urban_areas(cb = T,year = 2018)
fl_urban = urban[grepl('FL',urban$NAME10),]
urban_pops = fread('../../../Downloads/ACSDT1Y2019.B01003_2021-03-10T170752/ACSDT1Y2019.B01003_data_with_overlays_2021-03-07T163952.csv')
urban_pops = urban_pops[urban_pops$GEO_ID %in% fl_urban$AFFGEOID10,]

fl_urban$population = as.numeric(urban_pops$B01003_001E[match(fl_urban$AFFGEOID10,urban_pops$GEO_ID)])
fl_urban = fl_urban[!is.na(fl_urban$population),]
fl_urban$NAME10 = str_remove(fl_urban$NAME10,'\\,.*')
lab_centroids = st_centroid(fl_urban)

fl_urban$NAME10[grepl('Palm Coast',fl_urban$NAME10 )]<- 'Daytona Beach'


(f2 = ggplot() + geom_sf(data =florida,fill = 'white',col = 'grey75')+
  geom_sf(data = fl_urban[fl_urban$population>4e5,],fill = 'grey70',col = 'grey50') + #fill = scales::viridis_pal()(1)) +
  geom_label_repel(min.segment.length = 0.4,data =as.data.frame(st_coordinates(lab_centroids)[fl_urban$population>4e5,]),aes(x = X,y = Y,label = fl_urban$NAME10[fl_urban$population>4e5]),size = 2) + 
    theme_map() +
  scale_fill_colorblind() + 
  ggtitle("B: Major urban areas (>400k population)"))

f3 = ggplot() + geom_sf(data = florida,aes(fill = N)) + theme_map() +
  ggtitle("C: # CDDs active per county, 2018")+
  scale_fill_viridis_c(na.value = 'grey80',option = 'A',
                       direction = -1,name = '# of active CDDs')+ 
  theme(legend.position = c(0.2,0.25))
  
f4 = ggplot() + geom_sf(data = florida,aes(fill = V2/1e6)) + theme_map() +
  ggtitle("D: Total debt held by CDDs per county, 2018")+
  scale_fill_viridis_c(na.value = 'grey80',option = 'A',
                       direction = -1,name = 'Total debt ($1M)') + 
  theme(legend.position = c(0.2,0.25))
require(gridExtra)
f_all = grid.arrange(f1,f2,f3,f4,ncol = 2,top = 'Florida county population and CDDs, 2018')

ggsave(plot = f_all,filename = 'output/slgr/slgr_county_details.png',units = 'in',width= 7,height = 7,dpi = 700)

