base = 'https://www.nodc.noaa.gov/archive/arc0069/0121254/1.1/data/0-data/'
library(rvest)

sess = base %>% html_session()
links = sess %>% html_nodes('a') %>% html_text()
i = 0
while(i < length(links))
{
i = i + 1
if(i %in% which(grepl('_CCAP',(sess %>% html_nodes('a') %>% html_text()))))
{
temp = sess %>% follow_link(i = i) 
sapply(grep('\\.img$',(temp %>% html_nodes('a') %>% html_attr('href')),value=T),function(link)
download.file(url = paste0(temp$url,link),
              destfile = paste0('spatial_input/State_CCAPS/',link)))
}
}


test = sess %>% follow_link(i = 7) 



