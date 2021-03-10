



      
rev_source_melt = melt(rev_sources_tax_fee_assessment,id.vars = c('ENTITY_NAME','ENTITY_ID','YEAR','Age_In_Years','TOTALREV2'),variable.name = 'rev_type',value.name = 'rev_amount')
rev_source_melt$rev_amount[is.na(rev_source_melt$rev_amount)]<-0
rev_source_melt$Prop_Rev = rev_source_melt$rev_amount/rev_source_melt$TOTALREV2
rev_source_melt = rev_source_melt[Prop_Rev!=Inf]
temp = rev_source_melt[,mean(Prop_Rev,na.rm=T),by = .(Age_In_Years,rev_type)][Age_In_Years>=1]
ggplot(data = temp) + geom_line(aes(x = Age_In_Years,y = V1,col = rev_type)) +
  scale_y_continuous(limits=c(0,1)) + theme_bw() + ggtitle('Prop. revenue, average by CDD age')



service_revenue[!duplicated(SPEC_CAT_DESC),]                   
                      


grepl('Service Charge|Special Assess|Charge|Fee',DESCRIPTION),]


service_revenue[!duplicated(DESCRIPTION)]
as.data.table(table(rev$DESCRIPTION[rev$Gov_Type=='SD']))[order(-N),][1:20,]





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


