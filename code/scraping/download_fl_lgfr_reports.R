library(RSelenium)
library(rvest)
base = 'https://apps.fldfs.com/LocalGov/Reports/'
sess = html_session(base)
library(stringr)
report_types = sapply(0:9,function(x) base %>% read_html() %>% html_nodes(css = paste0('#ctl00_BodyHolder_RadioReports_',x)) %>% html_attr('value'))
years = 1993:2018
quer_grid = expand.grid('https://apps.fldfs.com/LocalGov/Reports/viewreports.aspx?ReportName=',report_types,'&YR=',years, '&ExportType=CSV',stringsAsFactors = F)
quer_grid = data.table(quer_grid)
setnames(quer_grid,c('Var2','Var4'),c('Report','Year'))
quer_grid$Report[quer_grid$Report=='TOTALREVEXPDEBT'] <- 'TOTALSREPORT'

quers = apply(quer_grid,1,function(x) paste(x,collapse=''))


rev_list = lapply(which(quer_grid$Report=='REVENUEDETAILREPORT'),function(i) {
  td = fread(getURL(quers[i]));td$Year = quer_grid$Year[i];td
})
revdt = rbindlist(rev_list)
fwrite(revdt,'input/florida_lgfr/florida_lg_revenue_1993-2018.csv')

unique(quer_grid$Report)

exp_list = lapply(which(quer_grid$Report=='EXPENDITUREDETAILREPORT'),function(i) {
  td = fread(getURL(quers[i]));td$Year = quer_grid$Year[i];td
})
expdt = rbindlist(exp_list)
fwrite(expdt,'input/florida_lgfr/florida_lg_expenditure_1993-2018.csv')


tot_revexpdebt_list = lapply(which(quer_grid$Report=='TOTALSREPORT'),function(i) {
  td = fread(getURL(quers[i]));td$Year = quer_grid$Year[i];td
})

tot_revexpdebt_dt = rbindlist(tot_revexpdebt_list )
fwrite(tot_revexpdebt_dt,'input/florida_lgfr/florida_lg_totrevexpdebt_1993-2018.csv')

revdt = fread('input/florida_lgfr/florida_lg_revenue_1993-2018.csv')
revdt = revdt[grepl('Development District',revdt$ENTITY_NAME),]


https://apps.fldfs.com/LocalGov/Reports/viewreports.aspx?ReportName=TOTALSREPORT&YR=2013&ExportType=EXCEL
https://apps.fldfs.com/LocalGov/Reports/viewreports.aspx?ReportName=TOTALREVEXPDEBT&YR=2015&ExportType=CSV
tot_revexpdebt_dt
sort(unique(revdt$DESCRIPTION))
revdt[,ACCOUNT_CD:=NULL]
revdt[,SPEC_CAT_ID:=NULL]
revdt[,GEN_CAT_ID:=NULL]
revdt[,DESCRIPTION:=NULL]
revdt[,Year:=NULL]

rev_long = melt(revdt,id.vars = c('ENTITY_ID','ENTITY_NAME','YEAR','SPEC_CAT_DESC','GEN_CAT_DESC'))
rev_long$value <- as.numeric(gsub('\\,','',rev_long$value))



revdt
test = 
test

rev_long$variable

sum(rev_long$value,na.rm = T)
unique(revdt$)

dcast(rvedt,ENTITY_ID + ENTITY_NAME + )
setkey(revdt,)
test = merge(tot_revexpdebt_dt,revdt)



dim(test)
dim(tot_revexpdebt_list)

library(RCurl)
library(data.table)
fread(RCurl::getURL(quers[1]))



quers
quers[1,]

quers$Var1






paste(quers[1,],collapse='')

str(test)

test %>% html_attr('value')


test[[1]]$doc
tx = 1


sapply(0:9,function(x) {
  x = 1
year_choice = remDr$findElement('id',value = paste0('ctl00_BodyHolder_RadioYears_',x))
year_choice$clickElement()
data_choice = remDr$findElement('id',value = paste0('ctl00_BodyHolder_RadioReports_',x))
data_choice$clickElement()
format_choice = remDr$findElement('id',value = paste0('ctl00_BodyHolder_RadioReportType_1'))
format_choice$clickElement()
remDr$click(buttonId = 'ctl00_BodyHolder_ImageButton1')


https://apps.fldfs.com/LocalGov/Reports/viewreports.aspx?ReportName=REVACCOUNTS&YR=2013&ExportType=XML



'https://apps.fldfs.com/LocalGov/Reports/viewreports.aspx?ReportName=REVACCOUNTS&YR=2013&ExportType=Excel'
https://apps.fldfs.com/LocalGov/Reports/viewreports.aspx?ReportName=REVENUEDETAILREPORT&YR=2013&ExportType=XML
https://apps.fldfs.com/LocalGov/Reports/viewreports.aspx?ReportName=REVENUEDETAILREPORTUNCERTIFIED&YR=2013&ExportType=XML


submit_button = remDr$findElement('id',value = 'ctl00_BodyHolder_ImageButton1')
submit_button$clickElement()


remDr$get

}

remDr$close()
pJS$stop()



year_choice = remDr$findElement('id',value = 'ctl00_BodyHolder_RadioYears_8')
year_choice$clickElement()

remDr$screenshot(useViewer = T,display = T)



remDr$click('#ctl00_BodyHolder_RadioYears_0')


remDr$screenshot(useViewer = T,display = T)

data_choice = remDr$findElement('css selector',value = '#ctl00_BodyHolder_RadioYears_9')
data_choice$buttondown()

file_choice = remDr$findElement('css selector',value = '#ctl00_BodyHolder_RadioReportType_1')
file_choice$buttondown()
remDr$click(buttonId = '#ct100__BodyHolder_ImageButton1')



nepaRadio = remDr$findElement(using = 'id',value = )




?remDr$screeenshot
