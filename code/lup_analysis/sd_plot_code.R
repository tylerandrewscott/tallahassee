


base = 'http://www.txcip.org/tac/census/sd.php?FIPS='
library(rvest)
library(tidyverse)
library(pbapply)
library(lubridate)
#scrape tax rates
# fips = read_csv('input/PEP_2012_PEPANNRES_with_ann.csv',skip = 1) %>% select(Id2) 

# tx_county_dists = pblapply(1:nrow(fips),function(i) paste0(base,fips$Id2[i]) %>% read_html() %>% html_node('table') %>% html_table(header = T,trim= T)  %>% mutate(FIPS = fips$Id2[i]))
# tx_dists = do.call(rbind,tx_county_dists)
# write_csv(tx_dists,'../windows-home/bosque/input/sd_2016_taxrates.csv')

library(tidyverse)
tx_dists = read_csv('../windows-home/bosque/input/sd_2016_taxrates.csv')
tx_dists$TYPE = NA
tx_dists$TYPE[grepl('Hospital|Health|Medical',tx_dists$`Special District Name`)] <- "Hospital"
tx_dists$TYPE[grepl('Municipal Utility|MUD|Municipal Ut',tx_dists$`Special District Name`)] <- "Municipal Utility District"
tx_dists$TYPE[grepl('Water|Groundwater|River|WCD|WCID|FWSD|WA|Conservation',tx_dists$`Special District Name`)] <- "Water"
tx_dists$TYPE[grepl('College|Education|School|Vocational',tx_dists$`Special District Name`)] <- "Education"
tx_dists$TYPE[grepl('Road|Transportation|Port',tx_dists$`Special District Name`)] <- "Transportation/Road"
tx_dists$TYPE[grepl('Improvement|Levee|Drainage|Flood|Irrigation|Navigation|Seawall|Reclamation',tx_dists$`Special District Name`)] <- "Flood/Drainage"
tx_dists$TYPE[grepl('Emergency',tx_dists$`Special District Name`)] <- "Emergency Services"
tx_dists$TYPE[grepl('Industrial|development|Development',tx_dists$`Special District Name`)] <- "Business/Industrial"
tx_dists$TYPE[grepl('Metro Park|Limited District',tx_dists$`Special District Name`)] <- "Business/Industrial"

library(readxl)
df_co = read_excel('input/CO SD Figures.xlsx') 
df_fl = read_excel('input/FL districts by type.xlsx')
df_tx = tx_dists %>% 
  rename(NAME = `Special District Name`) %>%
  mutate(TYPE = ifelse(grepl('Municipal Utility|MUD|Municipal Ut',NAME),'Municipal Utility District',TYPE)) %>%
select(-`Total Tax Rate, 2016` ,-`Total Levy, 2016`) %>%
  mutate(NAME = toupper(gsub('#','',NAME))) %>%
  mutate(NAME = gsub('MUNICIPAL UTILITY DISTRICT','MUD',NAME))

library(ggthemes)
df = rbind(
df_co %>% select(X__1,`2015`) %>% rename(Function = X__1) %>% mutate(STATE = 'CO'),
df_tx %>% group_by(TYPE) %>% summarise(`2015` = n()) %>% rename(Function = TYPE) %>% mutate(STATE = 'TX'),
df_fl %>% select(Function, `2015`) %>% arrange(- `2015`) %>% filter(!is.na(Function)) %>%
  filter(Function!='Total') %>%
  #mutate(Function = ifelse(`2015`>40,Function,'Other'))  %>%
  group_by(Function) %>% summarise(`2015` = sum(`2015`)) %>% mutate(STATE='FL'))
df$Function = gsub('\\n|\\r','',df$Function)
df$METRO_DISTRICT = grepl('Metropolitan|Municipal Util|Community Dev',df$Function) + 0

df = df %>% filter(!is.na(Function))

df$Function[grepl('Fire|Emergency|Ambulance',df$Function)] <- 'Emergency Services'
df$Function[grepl('Airport|Port|Transportation|Bridge|Road|Infrastructure',df$Function)] <- 'Transportation'
df$Function[grepl('Park|Beach|Recreat',df$Function)] <- 'Recreation'
df$Function[grepl('Community Dev|Municipal Utility|Metropolitan',df$Function)] <- 'Metropolitan'
df$Function[grepl('Business|Industri|Redevelop|Economic|Enterprise|Enhancement|Research|Improveme|Downtown|County Development',df$Function)] <- 'Business/Development'
df$Function[grepl('Water|Sewer|Sanitation|Wastewater|Solid|Lighting|Gas|Utility System',df$Function)] <- 'Utilities'
df$Function[grepl('Health|Hospital|Nursing|Housing|Education|Client|Childrens|Juvenile|Human',df$Function)] <- 'Health/Human Services'
df$Function[grepl('Dam|Infrastructure|Lake|River|Flood|Mosquito|Aquatic',df$Function)] <- 'Flood Control'
df$Function[grepl('Erosion|Land|Planning|Historic|Conservation|Environ',df$Function)] <- 'Conservation/Planning'
df$Function[grepl('Art|Library|Maintenance|Licensing|Personnel|Civic',df$Function)] <- 'Other'


df_temp = df %>% group_by(STATE,METRO_DISTRICT) %>% summarise(Total = sum(`2015`))
df_temp$STATE = rep(c('Colorado','Florida','Texas'),each=2)
gg1 = ggplot(df_temp) + geom_bar(aes(x = STATE,y = Total,fill = as.factor(METRO_DISTRICT)),
                                 stat='identity',position = 'dodge',width = 0.5) + theme_tufte(ticks=F) +
  scale_fill_grey(name = '',labels = c('All other special districts','Metropolitan service districts')) +
  scale_y_continuous(name = '# of districts') +
  theme(axis.text = element_text(size=12),axis.title.y = element_text(size=14),
        axis.title.x = element_blank(),legend.title = element_blank(),
        legend.text = element_text(size=12),title = element_text(size=14),
        legend.position = c(0.8,0.2),legend.background = element_rect(fill=alpha('white', 0.8))) + 
    #scale_x_discrete(labels=c('Colorado','Florida','Texas',expand=c(0,0))) +
  ggtitle('Relative frequency of metropolitan districts (2015)')


###### DISTRICT INFO ########

info = read_csv('../windows-home/bosque/input/texas_iwdd/infopage_wide.csv') %>% rename(SYSTEM_NAME = X2,ID = `District:`) %>% select(-X3,-X4)
info$STATUS = ifelse(grepl('[A-Z]',info$`Business Phone:`),info$`Business Phone:`,info$`Activity Status:`)
info$DISTRICT_TYPE <- NA
info$DISTRICT_TYPE[is.na(info$`Type:`)] <- info$`Activity Status:`[is.na(info$`Type:`)]
info$DISTRICT_TYPE[!grepl('[0-9]{3,}',info$`Business Phone:`)] <- info$`Activity Status:`[!grepl('[0-9]{3,}',info$`Business Phone:`)]
info$DISTRICT_TYPE[is.na(info$DISTRICT_TYPE)] <- info$`Type:`[is.na(info$DISTRICT_TYPE)]
info$DISTRICT_TYPE[grepl('WCID',info$SYSTEM_NAME)] <-'WATER CONTROL AND IMPROVEMENT DISTR'
info$DISTRICT_TYPE[grepl("MUD",info$SYSTEM_NAME)] <- "MUNICIPAL UTILITY DISTRICT"
info$STATUS <- NA
info$STATUS[!grepl('[0-9]{3,}',info$`Business Phone:`)] <- info$`Business Phone:`[!grepl('[0-9]{3,}',info$`Business Phone:`)]
info$STATUS[is.na(info$STATUS)] <- info$`Activity Status:`[is.na(info$STATUS)] 
info$FORMED_BY <- info$`Registration Received:`
info$BOARD_SELECTION <- NA
info$BOARD_SELECTION <- info$`Number of Directors:`
info$BOARD_SELECTION <- ifelse(info$BOARD_SELECTION ==  "Elected by Precinct",'Elected',info$BOARD_SELECTION)
info$BOARD_MEMBERS <- info$`Way Created:`
info$COUNTY <- NA
info$COUNTY <- info$`Way Chosen:`
info$TAX_RATE <- NA
info$`Actual Date of Notice:` <- gsub('\\$ |,','',info$`Actual Date of Notice:`)
info$TAX_RATE <- as.numeric(info$`Actual Date of Notice:`)
info$LEVY_TAX <- (!is.na(info$TAX_RATE)) + 0
library(stringr)
info$NUM_COUNTIES <- 1 + sapply(ifelse(is.na(info$`Main County:`),list(NULL),str_split(info$`Main County:`,"\\s{2,}")),length)
info$MULTI_COUNTY <- (info$NUM_COUNTIES >1) + 0
temp = read_csv('../windows-home/bosque/input/tceq_audits/district_info.csv')
temp$SYSTEM_NAME <- info$SYSTEM_NAME[match(temp$SYSTEM_ID,info$ID)]
temp$`District Type:`[grepl('RIVER AUTHORITY',temp$SYSTEM_NAME) & temp$`District Type:` == 'OTHER'] <- 'RIVER AUTHORITY'
event_df <- data.frame(creation = mdy(temp$`Creation Date:`),
                       start = decimal_date(mdy(temp$`Creation Date:`)),
                       event = ifelse(temp$`Activity Status:` == 'ACTIVE',0,1),
                       end = mdy(temp$`Activity Date:`),
                       end2 = mdy(temp$`Dissolved Date:`),
                       id = temp$SYSTEM_ID,
                       status = temp$`Activity Status:`,
                       reason = temp$`Activity Reason:`) 
event_df <- event_df %>% mutate(fail_date = ifelse(event==0,NA,ifelse(!is.na(end2),as.character(end2),as.character(end))))
event_df$creation_year = year(event_df$creation)
event_df$deactivation_year = year(ymd(event_df$fail_date))
event_df$end = decimal_date(ymd(event_df$fail_date))
event_df$event = ifelse(is.na(event_df$fail_date),0,1)
event_df$time = ifelse(is.na(event_df$fail_date), decimal_date(mdy('12/31/2016')) - event_df$start,  decimal_date(ymd(event_df$fail_date)) - event_df$start)
event_df$SYSTEM_NAME <- info$SYSTEM_NAME[match(event_df$id,info$ID)]


dfl_tx = event_df %>% filter(grepl("MUD|MUNICIPAL UT",SYSTEM_NAME))  %>% filter(!is.na(creation_year))

dfl_time = rbind(
df_co %>% filter(X__1=='Metropolitan') %>% gather(Year,Total,-X__1) %>% select(-X__1) %>% mutate(State = 'CO'),
df_fl %>% filter(Function == 'Community Development') %>% gather(Year,Total,-Function) %>% 
  mutate(State = 'FL') %>% select(-Function) %>%
  filter(Year>=2000),
data.frame(Year = seq(2000,2015,5),Total = sapply(seq(2000,2015,5),function(i) 
  sum(is.na(dfl_tx$deactivation_year)&i>=dfl_tx$creation_year)+
  sum(!is.na(dfl_tx$deactivation_year)&dfl_tx$creation_year <= i & i < dfl_tx$deactivation_year)),
  State = 'TX'))



gg2 = ggplot(dfl_time,aes(x = Year,y = Total,group = State,linetype = State)) + 
  geom_path() + geom_point() + theme_tufte(ticks=F) +
  scale_x_discrete(expand=c(0.05,0.05)) +
  scale_linetype(labels=c('Colorado','Florida','Texas')) +
  scale_y_continuous(name = '# of districts',limits=c(0,1600)) +
  theme(axis.text = element_text(size=12),axis.title.y = element_text(size=14),
        axis.title.x = element_blank(),legend.title = element_blank(),
        legend.text = element_text(size=12),title = element_text(size=14),
        legend.position = c(0.8,0.15),legend.background = element_rect(fill=alpha('white', 0.8)))  +
  ggtitle('Growth of metropolitan districts (2000 to 2015)')

rm(list=ls())
library(gridExtra)
grid.arrange(gg1,gg2,ncol=2)
grid.arrange(gg1,gg2,ncol=1)
gg2

cog_97 = read_csv('input/cog_files/gid_all_1997.csv') %>% rename(STATE = `STATE AB` ) %>% filter(STATE %in% c('FL','CO','TX')) %>% mutate('METRO_DISTRICT' = NA) %>%
  filter(!grepl('HOUSING',NAME)) %>% 
  filter(!grepl('WATER SUPPLY|WSC',NAME)) %>% 
  filter(!grepl('SCHOOL|EDUCATION|EDUCTION',NAME)) %>%
  filter(!grepl('CORP$|CORPORATION|ASSOCIATION',NAME))%>%
  filter(!grepl(' INC$| INC | BOARD$| BOARD  ',NAME))
cog_97$METRO_DISTRICT <- (grepl(' MUD$| MUD ',cog_97$NAME) & cog_97$STATE== 'TX') + 0
cog_97$METRO_DISTRICT[cog_97$STATE=='CO'&grepl('METRO DIST|METROPOLITAN DIST|METROPOLTAN DIST|METROPOLIATAN DIST',cog_97$NAME)] <-1
cog_97$METRO_DISTRICT[cog_97$STATE == 'FL' & grepl('COMMUNITY DEV|COMMUNITY DIST|COMM DEV|COM DEV',cog_97$NAME)] <-1


cog_02 = read_table('input/cog_files/2002GID_Special_Districts.txt',col_names = F)  %>% rename(CITY = X4) %>% mutate(ZIP = str_extract(X5,'(?![A-Z]{2})[0-9]{5}'),STATE = str_extract(X5,'^[A-Z]{2}'),
                                                                                                                           FIPS = str_extract(X6,'[0-9]{5}')) %>% select(-X5,-X6) %>% 
  mutate(X1 = gsub('^ ','',X1)) %>% mutate(FUNCTION = str_extract(X1,'[0-9]{2}[A-Z]{1}[a-z]{1,}.*?( {2})')) %>% mutate(FUNCTION = gsub('^ | $','',FUNCTION)) %>%
  mutate(NAME = gsub('^[0-9]{1,}','',str_extract(X1,'^.*?(?=[0-9]{2}[A-Z]{1}[a-z]{1,})'))) %>% 
  select(-X1,-X3) %>% filter(STATE %in% c('FL','CO','TX')) %>% mutate('METRO_DISTRICT' = NA) %>%
  filter(!grepl('HOUSING',NAME)) %>% 
  filter(!grepl('WATER SUPPLY|WSC',NAME)) %>% 
  filter(!grepl('SCHOOL|EDUCATION|EDUCTION',NAME))  %>% 
  filter(!grepl('CORP$|CORPORATION|ASSOCIATION',NAME)) %>% 
  filter(!grepl(' INC$| INC | BOARD$| BOARD  ',NAME)) 
cog_02$METRO_DISTRICT <- (grepl(' MUD$| MUD |MUNICIPAL UTILITY DISTRICT',cog_02$NAME) & cog_02$STATE== 'TX') + 0
cog_02$METRO_DISTRICT[cog_02$STATE=='CO'&grepl('METRO DIST|METROPOLITAN DIST|METROPOLTAN DIST|METROPOLIATAN DIST',cog_02$NAME)] <-1
cog_02$METRO_DISTRICT[cog_02$STATE == 'FL' & grepl('COMMUNITY DEV|COMMUNITY DIST|COMM DEV|COM DEV',cog_02$NAME)] <-1
library(rvest)

cog_07 = Reduce(full_join,c(read_html('input/cog_files/gid_fl_2007.html') %>% html_nodes('table') %>% html_table(trim=T), 
read_html('input/cog_files/gid_co_2007.html') %>% html_nodes('table') %>% html_table(trim=T),
read_html('input/cog_files/gid_tx_2007.html') %>% html_nodes('table') %>% html_table(trim=T)))
cog_07 = cog_07 %>% as.tibble() %>% rename(STATE = `STATE_AB`) %>% filter(STATE %in% c('FL','CO','TX')) %>% mutate(NAME = gsub('  ',' ',NAME))%>%
  filter(grepl('HOUSING',NAME))
  filter(!grepl('WATER SUPPLY|WSC',NAME)) %>% 
  filter(!grepl('SCHOOL|EDUCATION|EDUCTION',NAME)) %>%
  filter(!grepl('CORP$|CORPORATION|ASSOCIATION',NAME)) %>%
  filter(!grepl(' INC$| INC | BOARD$| BOARD  ',NAME))
cog_07$METRO_DISTRICT <- (grepl(' MUD$| MUD |MUNICIPAL UTILITY DISTRICT',cog_07$NAME) & cog_07$STATE== 'TX') + 0
cog_07$METRO_DISTRICT[cog_07$STATE=='CO'&grepl('METRO DIST|METROPOLITAN DIST|METROPOLTAN DIST|METROPOLIATAN DIST',cog_07$NAME)] <-1
cog_07$METRO_DISTRICT[cog_07$STATE == 'FL' & grepl('COMMUNITY DEV|COMMUNITY DIST|COMM DEV|COM DEV',cog_07$NAME)] <-1

cog_12 = read_table('input/cog_files/Fin_GID_2012.txt',col_names = FALSE) %>% mutate(ID = str_extract(X1,'[0-9]{14}'),X1 = gsub('^[0-9]{14}','',X1)) %>%
  mutate(NAME = str_extract(X1,'^.{64}'),X1 = gsub('^.{64}','',X1)) %>% rename(COUNTY = X1) %>% filter(!is.na(NAME)) %>%
  filter(grepl('^06|^10|^44',ID)) %>% mutate(NAME = gsub(' {1,}$','',NAME)) %>% filter(grepl('^[0-9]{2}4',ID)) %>% 
  filter(!grepl('HOUSING',NAME)) %>% 
  filter(!grepl('WATER SUPPLY|WSC',NAME)) %>% 
  filter(!grepl('SCHOOL|EDUCATION|EDUCTION',NAME)) %>%
  filter(!grepl('CORP$|CORPORATION|ASSOCIATION',NAME)) %>%
  filter(!grepl(' INC$| INC | BOARD$| BOARD  ',NAME))
cog_12$STATE = NA
cog_12$STATE[grepl('^06',cog_12$ID)]<-'CO'
cog_12$STATE[grepl('^10',cog_12$ID)]<-'FL'
cog_12$STATE[grepl('^44',cog_12$ID)]<-'TX'
cog_12$METRO_DISTRICT <- (grepl(' MUD$| MUD |MUNICIPAL UTILITY DISTRICT',cog_12$NAME) & cog_12$STATE== 'TX') + 0
cog_12$METRO_DISTRICT[cog_12$STATE=='CO'&grepl('METRO DIST|METROPOLITAN DIST|METROPOLTAN DIST|METROPOLIATAN DIST',cog_12$NAME)] <-1
cog_12$METRO_DISTRICT[cog_12$STATE == 'FL' & grepl('COMMUNITY DEV|COMMUNITY DIST|COMM DEV|COM DEV',cog_12$NAME)] <-1


cog = do.call(rbind,list(cog_97 %>% group_by(STATE,METRO_DISTRICT) %>% summarise(count = n()) %>% mutate(Year = 1997),
cog_02 %>% group_by(STATE,METRO_DISTRICT) %>% summarise(count = n()) %>% mutate(Year = 2002),
cog_07 %>% group_by(STATE,METRO_DISTRICT) %>% summarise(count = n()) %>% mutate(Year = 2007),
cog_12 %>% group_by(STATE,METRO_DISTRICT) %>% summarise(count = n()) %>% mutate(Year = 2012)))
library(forcats)
cg = cog %>% spread(METRO_DISTRICT,count) %>% rename(OTHER_SD = `0`,METRO_SD = `1`) %>% mutate(TOTAL = OTHER_SD+METRO_SD) %>%
  mutate(Year = fct_rev(as.factor(Year)))




ggplot(cg) + 
  geom_bar(aes(x = as.factor(Year),group = STATE,y=TOTAL,fill=STATE),position = 'dodge',stat='identity') +
  geom_bar(aes(x = as.factor(Year),group = STATE,y=METRO_SD),position = 'dodge',stat='identity',alpha = 0.25,fill= 'black') +
  scale_x_discrete(expand=c(0,0)) + 
  coord_flip() + theme_bw()


table(cog_07$STATE)
table(cog_07$STATE,cog_07$METRO_DISTRICT)
grep('SCHOOL',cog_07$NAME,invert = F,value=T)

cog_07$NAME[cog_07$STATE=='TX'&cog_07$METRO_DISTRICT==0]

head(cog_07)
table(cog_07$)
grep('COLLE',cog_97$NAME,value=T)

test[,3:7]
test$X1[3]  

filter(grepl('HARRIS',X1))



cog_97


table(is.na(cog_97$METRO_DISTRICT),cog_97$`STATE AB`)
table(is.na(cog_97$FUNCTION))


table(cog_97$FUNCTION[cog_97$METRO_DISTRICT==1])





