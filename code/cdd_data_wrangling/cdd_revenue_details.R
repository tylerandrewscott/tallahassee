library(data.table)

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
rev$Gov_Type = ifelse(grepl('^2',rev$ENTITY_ID),'City','SD')
rev$TOTAL_REVENUE = rowSums(rev[,10:19],na.rm=T)
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

utility_service_tax_codes = c(314100,314300, 314400, 314700	, 314800,314900	)
utility_franchise_fee_codes = c(323100, 323200,    323300	,  323400	, 323500	,  323600	,   323700	, 323900	)
utility_federal_grant_codes = c(331310, 331320	, 331330	,331340	,  331350	)
utility_state_grant_and_revshare_codes = c(334310, 334320,  334330	, 334340	, 334350,
                                           335310	,   335320	,   335330	,  335340	,335350	)
utility_service_charge_codes = c(343100,   343200	,  343300	, 343400	, 343500	,  343600	)


util_vals = as.character(as.vector(unlist(sapply(grep('^utility.*codes$',ls(),value=T),get))))
util_rev = rev_comb[as.character(ACCOUNT_CD) %in% util_vals,]
util_rev$SOURCE = NA
util_rev$SOURCE[util_rev$ACCOUNT_CD %in% utility_service_tax_codes] <- 'Service Tax'
util_rev$SOURCE[util_rev$ACCOUNT_CD %in% utility_service_charge_codes] <- 'Service Charge'
util_rev$SOURCE[util_rev$ACCOUNT_CD %in% utility_federal_grant_codes] <- 'Federal Grant'
util_rev$SOURCE[util_rev$ACCOUNT_CD %in% utility_state_grant_and_revshare_codes] <- 'State Grant/Rev. Share'
util_rev$SOURCE[util_rev$ACCOUNT_CD %in% utility_franchise_fee_codes] <- 'Franchise Fees'
util_rev_sum = util_rev[,sum(TOTAL_REVENUE),by = .(ENTITY_ID,ENTITY_NAME,YEAR,SOURCE)]
util_rev_sum$SOURCE = paste('Utility',util_rev_sum$SOURCE)
utility_revenue_sources = dcast(util_rev_sum,ENTITY_NAME + ENTITY_ID + YEAR ~ SOURCE,value.var = 'V1',fill = 0)
fwrite(utility_revenue_by_source_type,'input/florida_lgfr/utility_revenue_by_source_type.csv')


util_rev$UTILITY = NA
util_rev$UTILITY[grepl('Water|Sewer',util_rev$ACCOUNT_DESCRIPTION)] <- 'REVENUE Water/Sewer'
util_rev$UTILITY[grepl('Electr',util_rev$ACCOUNT_DESCRIPTION)] <- 'REVENUE Electricity'
util_rev$UTILITY[grepl('Cable|Telecomm',util_rev$ACCOUNT_DESCRIPTION)] <- 'REVENUE Telecommunications'
util_rev$UTILITY[grepl('',util_rev$ACCOUNT_DESCRIPTION)] <- 'REVENUE Water/Sewer'

util



unique(util_rev$ACCOUNT_DESCRIPTION)





#gaz = fread('input/uscb_gazetteer/2018_Gaz_place_national.txt')
#gaz = gaz[USPS=='FL',]

rev = fread('input/florida_lgfr/florida_lg_revenue_1993-2018.csv')
# city IDs start with 2, SD IDs start with 3, counties start with 1, other with 5
rev = rev[grepl('^(2|3)',rev$ENTITY_ID),]
#rev = rev[grepl('CDD|COMMUNITY DEVELOPMENT DIST',toupper(ENTITY_NAME)),]
replace_commas = data.table(apply(rev[,GENERAL:COMPONENT_UNITS],2,function(x) as.numeric(gsub("\\,",'',x))))
rev[,10:19] = replace_commas
rev[,Year:=NULL]
rev$Gov_Type = ifelse(grepl('^2',rev$ENTITY_ID),'City','SD')
rev$TOTAL_REVENUE = rowSums(rev[,10:19],na.rm=T)
#rev[Gov_Type == 'City'|grepl('CDD|Community Development District',ENTITY_NAME),]

rev$ENTITY_NAME = gsub('\\s{1,}',' ',rev$ENTITY_NAME)
rev$ENTITY_NAME = gsub('Comm\\.','Community',rev$ENTITY_NAME)
rev$ENTITY_NAME = gsub('Developoment|Deveopment|Dvlpmnt|Develoment|Developmente','Development',rev$ENTITY_NAME)
rev$ENTITY_NAME = gsub('CommunityDevelopment Dist\\.|Community(\\s|\\.)Develop\\. Dist\\.|Community(\\s|\\.)Develop(\\s|\\.)District|Community Dev(\\.\\s|\\s)Dist(\\.|$|\\s)|Community(\\s|\\.)Development(\\s|\\.)District|Community(\\s|\\.)Development(\\s|\\.)Dist(\\.$|$|\\s)|Community Develop\\. District|Community Dev\\. District|Community(\\s|\\.)Develop(\\s|\\.)Dist($|\\s)|Community(\\s|\\.)District$|Community(\\s|\\.)Development(\\s|\\.)Distr$','CDD',rev$ENTITY_NAME)
rev$ENTITY_NAME[rev$ENTITY_NAME=="Crossings at Fleming Island CDD, The"] <- "Crossings at Fleming Island CDD"

rev = rev[grepl("CDD",ENTITY_NAME)|Gov_Type=='City',]
library(forcats)
rev$GEN_CAT = as.factor(rev$GEN_CAT_DESC)
rev$GEN_CAT = fct_other(rev$GEN_CAT,drop = c("Sales","Interest and Other Earnings","Other Sources-Transfers" ,"Judgments, Fines and Forfeits" ,"Court-Related Revenues" , "Miscellaneous Revenues"),other_level = 'Other Sources')
rev$GEN_CAT = fct_other(rev$GEN_CAT, drop = c("State Grants","Federal Grants" ,"State Shared","Federal Payments in Lieu of Taxes", "State Payments in Lieu of Taxes" ),other_level = 'State and Federal Grants and Payments')
rev$GEN_CAT = fct_other(rev$GEN_CAT, drop = c("Local Payments in Lieu of Taxes","Local Government Unit Grants","Shared Local Government Unit Grants" ),other_level = 'Local Grants and Payments')
rev$GEN_CAT = fct_recode(rev$GEN_CAT,'Permits, Fees and Licenses' = 'Special Assessments/Impact Fees')


utility_revenue = rev[grepl('Water|Electric|Stormwater|Wastewater|Sewer|Telecomm|Broadband|Internet|Gas|Oil|Utility Service|Cable',DESCRIPTION),]
utility_revenue_by_source_type = utility_revenue[,sum(TOTAL_REVENUE),by = .(ENTITY_ID,ENTITY_NAME,YEAR,GEN_CAT_DESC,Gov_Type)]
utility_revenue_by_source_type$GEN_CAT_DESC = fct_recode(as.factor(utility_revenue_by_source_type$GEN_CAT_DESC), 
                                                                   'Utility State/Federal Grants'= c('Federal Grants'),
                                                                   'Utility State/Federal Grants'= c('State Grants'),
                                                                   'Utility State/Federal Grants'= c('State Shared'),
                                                                   'Utility Franchise Fees' = c('Permits, Fees and Licenses'),
                                                                   'Utility Taxes' = c('General Government Taxes'),
                                                                   'Utility Service Charges'  = 'Service Charges')
utility_revenue_by_source_type = utility_revenue_by_source_type[,sum(V1),by=.(ENTITY_ID,ENTITY_NAME,YEAR,GEN_CAT_DESC,Gov_Type)]
utility_revenue_by_source_type = dcast(utility_revenue_by_source_type ,ENTITY_ID  + ENTITY_NAME + Gov_Type + YEAR ~ GEN_CAT_DESC,value.var = 'V1',fill = 0)
fwrite(utility_revenue_by_source_type,'input/florida_lgfr/utility_revenue_by_source_type.csv')




unique(rev$SPEC_CAT_ID[grepl('Special Assessments - Charges for Public Services|Special Assessments - Service Charges',rev$DESCRIPTION)])


as.data.table(table(rev$DESCRIPTION[rev$Gov_Type=='SD']))[order(-N)][1:20,]



list(revenue_water= rev[grepl('Water|Stormwater|Wastewater|Sewer',DESCRIPTION)|grepl('Water|Stormwater|Wastewater|Sewer',SPEC_CAT_DESC),],
revenue_energy= rev[grepl('Gas|Elect|Oil',DESCRIPTION)|grepl('Gas|Elect|Oil',SPEC_CAT_DESC),],
revenue_telecom= rev[grepl('Telecomm|Cable|Internet|Broadband',DESCRIPTION)|grepl('Telecomm|Cable|Internet|Broadband',SPEC_CAT_DESC),],
revenue_transportation = rev[grepl('Transit|Transport|Road',DESCRIPTION)|grepl('Transit|Transport|Road',SPEC_CAT_DESC),],
revenue_parksrecreation = rev[grepl('Parks|Recreation|Cultur',DESCRIPTION)|grepl('Parks|Recreation|Cultur',SPEC_CAT_DESC),],
revenue_solidwaste = rev[grepl('Solid|Garbage',DESCRIPTION)|grepl('Solid|Garbage',SPEC_CAT_DESC),]



revenue_property_tax = rev[grepl('Ad Valorem',DESCRIPTION),],
revenue_property_tax = rev[grepl('Ad Valorem',DESCRIPTION),],



revenue_solidwaste = rev[grepl('Solid|Garbage',DESCRIPTION)|grepl('Solid|Garbage',SPEC_CAT_DESC),]


unique(grep('Impact',rev$DESCRIPTION,value=T))
unique(utility_revenue2$DESCRIPTION)
utility_revenue2$Utility_Service = NA
utility_revenue2$Utility_Service[grepl('Energy',utility_revenue)]


exp = fread('input/florida_lgfr/florida_lg_expenditure_1993-2018.csv')
exp = exp[grepl('^(2|3)',exp$ENTITY_ID),]
exp$Gov_Type = ifelse(grepl('^2',exp$ENTITY_ID),'City','SD')
exp[,OBJECT_CD:=NULL]
exp[,EXP_FUNCTION_CD:=NULL]
exp[,ACCOUNT_CD:=NULL]
exp[,Year:=NULL]
#exp = exp[grepl('CDD|COMMUNITY DEVELOPMENT DIST',toupper(ENTITY_NAME)),]
exp[,7:16] =data.table(apply(exp[,GENERAL:COMPONENT_UNITS],2,function(x) as.numeric(gsub("\\,",'',x))))
exp$TOTAL_EXPENDITURE = rowSums(exp[,7:16],na.rm=T)
exp$ENTITY_NAME = gsub('\\s{1,}',' ',exp$ENTITY_NAME)
exp$ENTITY_NAME = gsub('Comm\\.','Community',exp$ENTITY_NAME)
exp$ENTITY_NAME = gsub('Developoment|Deveopment|Dvlpmnt|Develoment|Developmente','Development',exp$ENTITY_NAME)
exp$ENTITY_NAME = gsub('Comm Dev Dist|Comm Development Dist|Commnty Dev Dist|CommunityDevelopment Dist\\.|Community(\\s|\\.)Develop\\. Dist\\.|Community(\\s|\\.)Develop(\\s|\\.)District|Community Dev(\\.\\s|\\s)Dist(\\.|$|\\s)|Community(\\s|\\.)Development(\\s|\\.)District|Community(\\s|\\.)Development(\\s|\\.)Dist(\\.$|$|\\s)|Community Develop\\. District|Community Dev\\. District|Community(\\s|\\.)Develop(\\s|\\.)Dist($|\\s)|Community(\\s|\\.)District$|Community(\\s|\\.)Development(\\s|\\.)Distr$','CDD',exp$ENTITY_NAME)

library(forcats)
exp$EXP_CAT = as.factor(exp$EXP_FUNCTION_DESC)
exp$EXP_CAT = fct_other(exp$EXP_CAT,drop = c("General Court Administration","General Court Operations" ,"Circuit Court-Civil","Circuit Court-Family" ,"Circuit Court-Juvenile" ,"Circuit Court-Criminal","County Court-Criminal","County Court-Civil" ,"Circuit Court-Probate","County Court-Traffic"),other_level = 'Other Uses')
exp$EXP_CAT = fct_other(exp$EXP_CAT, drop = c("State Grants","Federal Grants" ,"State Shared","Federal Payments in Lieu of Taxes", "State Payments in Lieu of Taxes" ),other_level = 'State and Federal Grants and Payments')
exp$EXP_CAT = fct_other(exp$EXP_CAT, drop = c("Local Payments in Lieu of Taxes","Local Government Unit Grants","Shared Local Government Unit Grants" ),other_level = 'Local Grants and Payments')


exp_general  = exp[EXP_FUNCTION_DESC=='General Government',]
exp_general = exp_general[,.(ENTITY_ID,ENTITY_NAME,YEAR,Gov_Type,TOTAL_EXPENDITURE,OBJECT_CD_DESC)] 
exp_general = exp_general[,sum(TOTAL_EXPENDITURE),by = .(ENTITY_ID,ENTITY_NAME,YEAR,Gov_Type,OBJECT_CD_DESC)]
general_government_expenditures_by_object = dcast(exp_general , ENTITY_ID + ENTITY_NAME + YEAR + Gov_Type ~ OBJECT_CD_DESC,value.var = 'V1',fill = 0)
fwrite(general_government_expenditures_by_object ,'input/general_government_expenditures_by_object.csv')

exp_general  = exp[EXP_FUNCTION_DESC=='General Government',]
exp_general = exp_general[,.(ENTITY_ID,ENTITY_NAME,YEAR,Gov_Type,TOTAL_EXPENDITURE,DESCRIPTION)] 
exp_general = exp_general[,sum(TOTAL_EXPENDITURE),by = .(ENTITY_ID,ENTITY_NAME,YEAR,Gov_Type,DESCRIPTION)]
general_government_expenditures_by_function = dcast(exp_general , ENTITY_ID + ENTITY_NAME + YEAR + Gov_Type ~ DESCRIPTION,value.var = 'V1',fill = 0)
fwrite(general_government_expenditures_by_function ,'general_government_expenditures_by_function.csv')



 rev[grep('Tax',rev$DESCRIPTION),][!duplicated(DESCRIPTION)]


tax_revenue_by_source_type = 
rev[GEN_CAT_DESC%in%c( "Ad Valorem Taxes" , "General Government Taxes")& !grepl('Water|Electric|Stormwater|Wastewater|Sewer|Telecomm|Broadband|Internet|Gas|Oil|Cable',DESCRIPTION)]


non_utility_service_revenue = rev[!grepl('Water|Electric|Stormwater|Wastewater|Sewer|Telecomm|Broadband|Internet',DESCRIPTION)&GEN_CAT_DESC=='Service Charges',]
non_utility_service_revenue$SPEC_CAT_DESC <- fct_recode(non_utility_service_revenue$SPEC_CAT_DESC,'Other Service Charges' = 'Physical Environment',
                                                        'Other Service Charges' = 'Other Charges for Services',
                                                        'Other Service Charges' = 'Internal Services Fees and Charges',
                                                        
                                                        
                                                        )                  



service_revenue[!duplicated(SPEC_CAT_DESC),]                   
                      


grepl('Service Charge|Special Assess|Charge|Fee',DESCRIPTION),]


service_revenue[!duplicated(DESCRIPTION)]
as.data.table(table(rev$DESCRIPTION[rev$Gov_Type=='SD']))[order(-N),][1:20,]



rev[DESCRIPTION=='Special Assessments - Service Charges']



utility_revenue_by_source_type 

utility_revenue_by_source_type = dcast(utility_revenue_by_source_type[,sum(V1),by=.(ENTITY_ID,YEAR,GEN_CAT_DESC)],ENTITY_ID + YEAR ~ GEN_CAT_DESC,value.var = 'V1',fill = 0)






,'State Grants','State Shared'))



utility_tax_rev = rev[SPEC_CAT_DESC %in% c('Utility Service Taxes','Utility Service Charges'),][,.(ENTITY_ID,ENTITY_NAME,YEAR,DESCRIPTION,Gov_Type,TOTAL_REVENUE)][,sum(TOTAL_REVENUE),by = .(ENTITY_ID,YEAR)]
utility_tax_rev = rev[SPEC_CAT_DESC %in% c('Utility Service Taxes','Utility Service Charges'),][,.(ENTITY_ID,ENTITY_NAME,YEAR,DESCRIPTION,Gov_Type,TOTAL_REVENUE)][,sum(TOTAL_REVENUE),by = .(ENTITY_ID,YEAR)]



rev[grepl('Water',DESCRIPTION) & !grepl('Service Charge',GEN_CAT)]




{grepl('Electric|Water|Wastewater|Stormwater|Gas|Telecomm|Sewer',DESCRIPTION) & grepl('Charge',DESCRIPTION)} | 
  {grepl('Electric|Water|Wastewater|Stormwater|Gas|Telecomm|Sewer',DESCRIPTION) & grepl('Charge',DESCRIPTION)} | 


rev[grepl('Service Charge',DESCRIPTION)]

rev[grepl('Service Charge',DESCRIPTION),][!duplicated(DESCRIPTION)]

utility_charge_rev = rev[grepl('Service Charge',DESCRIPTION)&grepl('Utility|Electricity|Telecommunications|Solid Waste',DESCRIPTION),][,.(ENTITY_ID,ENTITY_NAME,YEAR,DESCRIPTION,Gov_Type,TOTAL_REVENUE)][,sum(TOTAL_REVENUE),by = .(ENTITY_ID,YEAR)]
setnames(utility_charge_rev,'V1','Utility_Service_Charges')


utility_tax_rev = rev[grepl('Tax',DESCRIPTION)&grepl('Utility|Electricity|Telecommunication|Solid Waste',DESCRIPTION),][,.(ENTITY_ID,ENTITY_NAME,YEAR,DESCRIPTION,Gov_Type,TOTAL_REVENUE)][,sum(TOTAL_REVENUE),by = .(ENTITY_ID,YEAR)]
setnames(utility_tax_rev,'V1','Utility_Service_Taxes')

rev[grepl('Tax',DESCRIPTION)&grepl('Utility|Electricity|Telecommunication|Solid Waste',DESCRIPTION),]
rev[]

utility_franchise_rev = rev[grepl('Franchise',DESCRIPTION)][,.(ENTITY_ID,ENTITY_NAME,YEAR,DESCRIPTION,Gov_Type,TOTAL_REVENUE)][,sum(TOTAL_REVENUE),by = .(ENTITY_ID,YEAR)]
setnames(utility_franchise_rev,'V1','Utility_Franchise_Fees')
utility_grant_rev = rev[grepl('Supply',DESCRIPTION)][,.(ENTITY_ID,ENTITY_NAME,YEAR,DESCRIPTION,Gov_Type,TOTAL_REVENUE)][,sum(TOTAL_REVENUE),by = .(ENTITY_ID,YEAR)]
setnames(utility_grant_rev,'V1','Utility_Grant_Receipts')
utility_rev = merge(merge(merge(utility_charge_rev,utility_tax_rev),utility_franchise_rev),utility_grant_rev,fill = 0)

contribs = rev[DESCRIPTION %in% grep('Contributions|Donations',DESCRIPTION,value=T),]
contribs$Type = NA   
contribs$Type[grepl('Contribution',contribs$DESCRIPTION)&grepl('Private',contribs$DESCRIPTION)] <- 'Private Contributions'
contribs$Type[grepl('Contribution|Donations',contribs$DESCRIPTION)&grepl('Federal|State',contribs$DESCRIPTION)] <- 'Federal/State Contributions'
contribs$Type[grepl('Contribution',contribs$DESCRIPTION)&grepl('Enterprise',contribs$DESCRIPTION)] <- 'Enterprise Ops Contributions'
contribs$Type[grepl('Contribution',contribs$DESCRIPTION)&grepl('Pension|Compen',contribs$DESCRIPTION)] <- 'Pension/Deferrment Contributions'
contribs$Type[grepl('Contribution|Donation',contribs$DESCRIPTION)&grepl('Other',contribs$DESCRIPTION)] <- 'Other Source Contributions'
contrib_rev = dcast(contribs[,.(ENTITY_ID,YEAR,TOTAL_REVENUE,Type)][,sum(TOTAL_REVENUE),by=.(ENTITY_ID,YEAR,Type)],ENTITY_ID + YEAR ~ Type,value.var = 'V1',fill = 0)

impactfees = rev[DESCRIPTION %in% grep('Impact',DESCRIPTION,value=T),]
impactfees$Type <- NA
impactfees$Type[grepl('Commercial',impactfees$DESCRIPTION)] <- 'Commercial Impact Fees'
impactfees$Type[grepl('Residential',impactfees$DESCRIPTION)] <- 'Residential Impact Fees'
impactfees$Type[!grepl('Residential|Commercial',impactfees$DESCRIPTION)] <- 'Other Impact Fees'
impact_rev = dcast(impactfees[,.(ENTITY_ID,YEAR,TOTAL_REVENUE,Type)][,sum(TOTAL_REVENUE),by=.(ENTITY_ID,YEAR,Type)],ENTITY_ID + YEAR ~ Type,value.var = 'V1',fill = 0)


rev[]
rev[DESCRIPTION=='Water',]
as.data.table(table(rev$DESCRIPTION))[order(-N),][1:10,]

impactfees
impactfees[is.na(Type)]

unique(impactfees$DESCRIPTION)
sort(unique(rev$DESCRIPTION))






contribs[,.(ENTITY_ID,YEAR,TOTAL_REVENUE,Type)][,sum(TOTAL_REVENUE),by=.(ENTITY_ID,YEAR,Type)]
dcast(contribs[,.(ENTITY_ID,YEAR,TOTAL_REVENUE,Type)][,sum(TOTAL_REVENUE),by=.(ENTITY_ID,YEAR,Type)],ENTITY_ID + YEAR ~ Type,value.var = 'TOTAL_REVENUE')




contribs[,.(ENTITY_ID,YEAR,TOTAL_REVENUE,Type)]
contribs[is.na(Type)][!duplicated(DESCRIPTION)]


contrib_private_rev = rev[grepl('Contribution',DESCRIPTION)&grepl('Private',DESCRIPTION),][,.(ENTITY_ID,ENTITY_NAME,YEAR,DESCRIPTION,Gov_Type,TOTAL_REVENUE)][,sum(TOTAL_REVENUE),by = .(ENTITY_ID,YEAR)]
contrib_private_rev = rev[grepl('Contribution',DESCRIPTION)&grepl('Federal|State',DESCRIPTION),][,.(ENTITY_ID,ENTITY_NAME,YEAR,DESCRIPTION,Gov_Type,TOTAL_REVENUE)][,sum(TOTAL_REVENUE),by = .(ENTITY_ID,YEAR)]
contrib_private_rev = rev[grepl('Contribution',DESCRIPTION)&grepl('Enterprise',DESCRIPTION),][,.(ENTITY_ID,ENTITY_NAME,YEAR,DESCRIPTION,Gov_Type,TOTAL_REVENUE)][,sum(TOTAL_REVENUE),by = .(ENTITY_ID,YEAR)]


unique(contribs$DESCRIPTION)
      
      [,sum(TOTAL_REVENUE),by = .(ENTITY_ID,YEAR,DESCRIPTION)]
      
      
dcast(rev[DESCRIPTION %in% grep('Contributions|Donations',DESCRIPTION,value=T),][,sum(TOTAL_REVENUE),by = .(ENTITY_ID,YEAR,DESCRIPTION)], ENTITY_ID + YEAR ~DESCRIPTION,value.var = 'V1')


contribution_private = rev[grepl('Service Charge',DESCRIPTION)&grepl('Utility',DESCRIPTION),][,.(ENTITY_ID,ENTITY_NAME,YEAR,DESCRIPTION,Gov_Type,TOTAL_REVENUE)][,sum(TOTAL_REVENUE),by = .(ENTITY_ID,YEAR)]




rev[grepl('Supply',DESCRIPTION),][!duplicated(DESCRIPTION)]
sort(unique(rev$DESCRIPTION))

do.call(merge,list(utility_charge_rev,utility_tax_rev,utility_franchise_rev),by = .(ENTITY_ID,YEAR))






water = rev[grepl('Water|Sewer|Wastewater|Stormwater',rev$DESCRIPTION)&!grepl('Port',rev$DESCRIPTION),][GEN_CAT_DESC!='State Grants',]




dcast(water[,.(ENTITY_ID,ENTITY_NAME,SPEC_CAT_DESC,GEN_CAT_DESC,DESCRIPTION,TOTAL_REVENUE,Gov_Type)], . ~ 
rev[,REVENUE:=sum(GENERAL:COMPONENT)]

rev_tot = rev[,sum(TOTAL_REVENUE),by = .(ENTITY_ID,ENTITY_NAME,YEAR,Gov_Type,GEN_CAT,)]
setnames(rev_tot,'V1','REVENUE')

sort(unique(rev$SPEC_CAT_DESC))
rev[GEN_CAT %in% ]

test = rev[SPEC_CAT_DESC%in%c('Physical Environment','Utility Service Taxes','Franchise Fees'),]



water


water$DESCRIPTION=='Fran'

unique(water$DESCRIPTION)

unique(test$DESCRIPTION)
rev[SPEC_CAT_DESC == 'General Government']


rev_tot$GEN_CAT = paste0('REVENUE ',rev_tot$GEN_CAT)
rev_cast = dcast(rev_tot,ENTITY_ID + ENTITY_NAME + YEAR + Gov_Type ~ GEN_CAT,value.var = 'REVENUE')



exp = fread('input/florida_lgfr/florida_lg_expenditure_1993-2018.csv')
exp = exp[grepl('^(2|3)',exp$ENTITY_ID),]
exp[,OBJECT_CD:=NULL]
exp[,EXP_FUNCTION_CD:=NULL]
exp[,ACCOUNT_CD:=NULL]
exp[,Year:=NULL]
#exp = exp[grepl('CDD|COMMUNITY DEVELOPMENT DIST',toupper(ENTITY_NAME)),]
exp[,7:16] =data.table(apply(exp[,GENERAL:COMPONENT_UNITS],2,function(x) as.numeric(gsub("\\,",'',x))))
exp$TOTAL_EXPENDITURE = rowSums(exp[,7:16],na.rm=T)
exp$ENTITY_NAME = gsub('\\s{1,}',' ',exp$ENTITY_NAME)
exp$ENTITY_NAME = gsub('Comm\\.','Community',exp$ENTITY_NAME)
exp$ENTITY_NAME = gsub('Developoment|Deveopment|Dvlpmnt|Develoment|Developmente','Development',exp$ENTITY_NAME)
exp$ENTITY_NAME = gsub('Comm Dev Dist|Comm Development Dist|Commnty Dev Dist|CommunityDevelopment Dist\\.|Community(\\s|\\.)Develop\\. Dist\\.|Community(\\s|\\.)Develop(\\s|\\.)District|Community Dev(\\.\\s|\\s)Dist(\\.|$|\\s)|Community(\\s|\\.)Development(\\s|\\.)District|Community(\\s|\\.)Development(\\s|\\.)Dist(\\.$|$|\\s)|Community Develop\\. District|Community Dev\\. District|Community(\\s|\\.)Develop(\\s|\\.)Dist($|\\s)|Community(\\s|\\.)District$|Community(\\s|\\.)Development(\\s|\\.)Distr$','CDD',exp$ENTITY_NAME)

library(forcats)
exp$EXP_CAT = as.factor(exp$EXP_FUNCTION_DESC)
exp$EXP_CAT = fct_other(exp$EXP_CAT,drop = c("General Court Administration","General Court Operations" ,"Circuit Court-Civil","Circuit Court-Family" ,"Circuit Court-Juvenile" ,"Circuit Court-Criminal","County Court-Criminal","County Court-Civil" ,"Circuit Court-Probate","County Court-Traffic"),other_level = 'Other Uses')
exp$EXP_CAT = fct_other(exp$EXP_CAT, drop = c("State Grants","Federal Grants" ,"State Shared","Federal Payments in Lieu of Taxes", "State Payments in Lieu of Taxes" ),other_level = 'State and Federal Grants and Payments')
exp$EXP_CAT = fct_other(exp$EXP_CAT, drop = c("Local Payments in Lieu of Taxes","Local Government Unit Grants","Shared Local Government Unit Grants" ),other_level = 'Local Grants and Payments')

exp_tot = exp[,sum(TOTAL_EXPENDITURE),by = .(ENTITY_ID,ENTITY_NAME,YEAR,EXP_CAT)]
setnames(exp_tot,'V1','EXPENDITURE')
exp_tot$EXP_CAT = paste0('EXPENDITURE ',exp_tot$EXP_CAT)
exp_cast = dcast(exp_tot,ENTITY_ID + ENTITY_NAME + YEAR  ~ EXP_CAT,value.var = 'EXPENDITURE')

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
setkey(fdf,ENTITY_NAME)
setkey(alldt,ENTITY_NAME)

alldt = fdf[alldt,]
install.packages('sf')
library(sf)
library(tigris)

gaz$NAME = gsub(' town$| city$','',gaz$NAME)

gaz
gaz$match(alldt$ENTITY_NAME,gaz$NAME)))
dim(alldt)
fdt = merge(fdf,alldt,all = T)


fnames = grep('EXP|REV',colnames(fdt),value=T)
fdt[,(fnames):=lapply(.SD,zoo::na.fill,fill = 0),.SDcols = fnames]
fdt$MATCHED_DATA = (!is.na(fdt$County)) + 0
fdt = fdt[order(-MATCHED_DATA),]
fwrite(fdt,'input/florida_sd_data/cdd_panel_1993-2018.csv')


table(is.na(fdt$County))
ggplot(fdt[fdt$Gov_Type=='SD',]) + geom_point(aes(x = YEAR,y = `REVENUE Contributions and Donations`))


table(fdt$MATCHED_DATA)
fdt$`REVENUE Contributions and Donations`
library(tidyverse)



