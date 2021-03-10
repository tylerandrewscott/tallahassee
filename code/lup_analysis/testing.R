require(rgdal)

library(tidyverse)

temp = read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSjykKij_RQ6uT6K3zIuEc0PxWhSEcS-PifLWOUmX7ryyEKIrgF0BKNiK8pFoL7h8nal9koPkb36UTc/pub?output=csv')

temp= temp %>% rename(County = `County(ies)`) %>% filter(Function == 'Community Development')

count = temp %>% group_by(County) %>% summarise(co = n()) %>% arrange(-co)  %>%
  rename(num_cdds = co)

count %>% head(20)

count[count$County %in% c('Leon','Manatee','Duval','Miami-Dade','Hillsborough','St. Johns'),]


temp[temp$County=='St. Lucie',]

  summarise(sum(co))

227/sum(count$co)
count


levels(sj@data$Name)
sj = readOGR('spatial_input/county_cdd','cdd')


test = readOGR('scratch_spatial/','ParcelData_20170831')

unique(test@data$TAXDISTDES)
head(test@data)
plot(test)


test@data$District
test@data$Name
levels(test$Zoned)
head(test@data)
test@data$Zoned
dim(sj@data)

sort(temp$`District's Name`[temp$County=='St. Johns'])[!sort(temp$`District's Name`[temp$County=='St. Johns']) %in% 
  paste(unique(sj@data$NAME),'Community Development District',sep=' ')]


unique(sj@data$NAME)as.data.frame(table(temp$County,temp$Function == 'Community Development'))

library(rgdal)
sj = readOGR('scratch_spatial','WaterServiceAreas')
sj@data$Name
levels(temp@data$ZN_TYPE)
plot(temp)

# The input file geodatabase
fgdb <- "spatial_input/Utility_SA_Current.gdb/"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
fc <- readOGR(dsn=fgdb,layer="UTILITY_SERVICE_AREA_CURRENT")
grep('Dev|DEV',fc@data$CUP_SA,value=T)




# Determine the FC extent, projection, and attribute information
summary(fc)

# View the feature class
plot(fc)





