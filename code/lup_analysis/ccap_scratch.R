library (sp)
library (rgdal)
library(raster)
library(SDMTools)
library(adehabitatMA)
library (maps)
library (mapproj)
library(ModelMap)
library(tidyverse)
fl = readOGR('spatial_input/government_units/','county_nrcs_a_fl')
fl_ccaps <- lapply(grep('fl_[0-9]{4}_ccap',list.files('spatial_input/'),value=T),function(x) raster(paste0('spatial_input/',x)))
fl_ccap_stack <- do.call(stack,fl_ccaps)
fl = spTransform(fl,CRS(proj4string(fl_ccap_stack)))
fl_sub <- fl[fl@data$COUNTYNAME %in% c('Miami-Dade','Manatee','Hillsborough','Leon','St. Johns','Bay'),]
fl_places <- readOGR('spatial_input/government_units','tl_2016_12_place')
fl_places <- fl_places[!grepl('CDP',fl_places@data$NAMELSAD),]
fl_places = spTransform(fl_places,CRS(proj4string(fl_ccap_stack)))

library(ggthemes)
class_matrix = rbind(c(0,1,NA),c(2,4,1),c(5,20,0),c(21,25,0))
fl_ccaps_stack_dev <- raster::reclassify(x = fl_ccap_stack,rcl = class_matrix,right = NA)
names(fl_ccaps_stack_dev) <- c("Developed 1996","Developed 2001","Developed 2006","Developed 2010")
#library(rasterVis)
#library(viridis)


size <- 0.5
hex_points <- spsample(study_area, type = "hexagonal", cellsize = size)
hex_grid <- HexPoints2SpatialPolygons(hex_points, dx = size)
plot(study_area, col = "grey50", bg = "light blue", axes = TRUE)
plot(hex_points, col = "black", pch = 20, cex = 0.5, add = T)
plot(hex_grid, border = "orange", add = T)
# comb <- fl_ccaps_stack_dev[[1]] + fl_ccaps_stack_dev[[2]] +fl_ccaps_stack_dev[[3]]  +fl_ccaps_stack_dev[[4]] 
# comb[comb == 0] <- NA
# fl_gg <- fortify(fl)
# gg1 <- gplot(comb) + 
#   geom_polygon(data = fl_gg,aes(x = long, y = lat,group = group),fill = 'grey90',colour = 'grey60')+
#   #geom_polygon(data = fl_gg,aes(x = long, y = lat,group = group),fill = 'grey80',colour = 'grey20') +
#   geom_tile(aes(fill = as.character(value)),na.rm=F,alpha = 1) +
#   #facet_wrap(~ variable) +
#   scale_fill_viridis(name = 'Developed land',discrete = T,direction = -1,option = 'C',
#                      labels = c('Developed 2006-2010','Developed 2001-2006',
#                                 'Developed 1996-2001','Developed pre-1996','')) +
#   coord_equal() + theme(rect = element_rect(fill = 'grey80'),axis.text = element_blank(),
#                                             axis.title = element_blank(),legend.position = c(0.3,0.3))

shape_files = unique(gsub('\\..*','',grep('CDD',list.files('spatial_input/county_cdd'),value=T)))
prefix = 'spatial_input/county_cdd'
cdd_manatee = spTransform(readOGR(prefix,'FL_Manatee_CDD'),CRS(proj4string(fl)))
cdd_duval = spTransform(readOGR(prefix,'FL_Duval_CDD'),CRS(proj4string(fl)))
cdd_leon = spTransform(readOGR(prefix,'FL_Leon_CDD'),CRS(proj4string(fl)))
cdd_stjohns = spTransform(readOGR(prefix,'FL_StJohns_CDD'),CRS(proj4string(fl)))
cdd_hillsborough = spTransform(readOGR(prefix,'FL_Hillsborough_CDD'),CRS(proj4string(fl)))
bay1 = readOGR('spatial_input/county_cdd','LakePowellCCD')
bay2 = readOGR('spatial_input/county_cdd','PierParkCCD')
bay3 = readOGR('spatial_input/county_cdd','SeahavenCCD')
cdd_bay = do.call(rbind,list(bay1,bay2,bay3))
cdd_bay = spTransform(cdd_bay,CRS(proj4string(fl)))
library(raster)

cdds <- do.call(bind,list(cdd_leon,cdd_stjohns,cdd_manatee,cdd_duval,cdd_hillsborough,cdd_bay))
#cdds <- cdd_hillsborough
#ccaps_stack_duval <- crop(fl_ccaps_stack_dev,fl_sub)
#ccap_shrink <- aggregate(fl_ccaps_stack_dev, fact=10, fun=mean, expand=FALSE, na.rm=TRUE)
ccap_shrink_more <- aggregate(fl_ccaps_stack_dev, fact=50, fun=mean, expand=FALSE, na.rm=TRUE)
p <- as(ccap_shrink_more, 'SpatialPixelsDataFrame')
p_in_county <- over(p,fl_sub)
p <- p[!is.na(p_in_county$OBJECTID),]
ov <- over(fl_places,fl_sub)
fl_places_sub <- fl_places[!is.na(ov$OBJECTID),]
p_over_cdd <-  over(p,cdds)
p_over_place <- over(p,fl_places_sub)
p@data$OVER_CDD <- (!is.na(p_over_cdd$OBJECTID))+0
p@data$OVER_PLACE <- (!is.na(p_over_place$GEOID))+0
which_county <- over(p,fl_sub)
p@data$COUNTY <- which_county$COUNTYNAME

library(INLA)
df <- data.frame(coordinates(p),p@data)
df <- df %>% mutate(ID = 1:nrow(df)) %>% gather(Year,Developed,-x,-y,-OVER_CDD,-OVER_PLACE,-ID,-COUNTY) %>% mutate(Year = gsub('Developed\\.','',Year)) %>%
  mutate(Period = ifelse(Year == 1996,1,ifelse(Year == 2001,2,ifelse(Year == 2006,3,4))))


fl_county_df <- haven::read_dta('input/fl_counties.dta')
df <- left_join(df,fl_county_df %>% rename(COUNTY = CountyName,Year = FromYear) %>% 
            mutate(Year = as.character(Year)) %>%
            select(COUNTY,Year,white,income,pop))

df = df %>% mutate(white_scaled = as.vector(scale(white)),income_scaled = as.vector(scale(income)),
                   pop_scaled = as.vector(scale(pop)))

bound <- inla.sp2segment(fl_sub)
mesh<-inla.mesh.create.helper(points = cbind(df$x,df$y),boundary = bound,#points.domain = coordinates(p),
                              max.edge=c(10000), cutoff=2500)

spde <- inla.spde2.pcmatern(mesh = mesh,alpha = 2,prior.range=c(0.3,0.5),prior.sigma = c(1,0.01))
#iset <- inla.spde.make.index('i', n.spde=spde$n.spde, n.group=4)
coords <- as.matrix(cbind(df$x,df$y))
A <- inla.spde.make.A(mesh = mesh, loc = coords)
df$Developed = as.numeric(df$Developed)

u <- (df$Developed > 0) + 0
y <- ifelse(u == 1,as.numeric(df$Developed), NA)
n = nrow(df)
idat <- list(Y=matrix(NA,2*n,2))
idat$Y[1:n,1] <- u
idat$Y[n+1:n,2] <- y
idat$mu.u <- rep(1:0, each=n)
idat$mu.y <- rep(0:1, each=n)
idat$u_cdd <- c(df$OVER_CDD, rep(0,n))
idat$y_cdd <- c(rep(0,n), df$OVER_CDD)
idat$u_place <- c(df$OVER_PLACE, rep(0,n))
idat$y_place <- c(rep(0,n), df$OVER_PLACE)
idat$u_period <- c(df$Period, rep(0,n))
idat$y_period <- c(rep(0,n), df$Period)
idat$u_county <- c(df$COUNTY, rep(NA,n))
idat$y_county <- c(rep(NA,n), df$COUNTY)
idat$u_income <- c(df$income_scaled, rep(NA,n))
idat$y_income <- c(rep(NA,n), df$income_scaled)
idat$u_population <- c(df$pop_scaled, rep(NA,n))
idat$y_population <- c(rep(NA,n), df$pop_scaled)
idat$u_white <- c(df$white_scaled, rep(NA,n))
idat$y_white <- c(rep(NA,n), df$white_scaled)
idat$y.i <- idat$u.i <- c(1:n, 1:n)
idat$u_ID = c(as.character(df$ID),rep(NA,n))
idat$y_ID = c(rep(NA,n),as.character(df$ID))

idat$u_county <- as.numeric(as.factor(idat$u_county))
idat$y_county <- as.numeric(as.factor(idat$y_county))

stk.u <- inla.stack(tag='est.u', data=list(u = u, ### occurrence for separate model 
                                           y=cbind(u, NA)), ### z at first column of y 
                    A=list(A, 1), effects=list( list(i.u=1:spde$n.spde), 
                                                list(u.b0=rep(1,length(u)),
                                                     u_cdd = idat$u_cdd[1:length(u)],
                                                     u_place = idat$u_place[1:length(u)],   
                                                     u_white = idat$u_white[1:length(u)],
                                                     u_income = idat$u_income[1:length(u)],
                                                     u_pop = idat$u_population[1:length(u)],
                                                     u_county = idat$u_county[1:length(u)],
                                                     u_period = idat$u_period[1:length(u)],
                                                     u_id = idat$u_ID[1:length(u)])))

stk.y <- inla.stack(tag='est.y', data=list(r = y, ### occyrrence for separate model 
                                           y=cbind(NA,y)), ### z at first colymn of y 
                    A=list(A, 1), effects=list( list(i.y=1:spde$n.spde), 
                                                list(y.b0=rep(1,length(y)),
                                                     y_cdd = idat$y_cdd[length(u)+1:length(y)],
                                                     y_place = idat$y_place[length(u)+1:length(y)],   
                                                     y_white = idat$y_white[length(u)+1:length(y)],
                                                     y_income = idat$y_income[length(u)+1:length(y)],
                                                     y_pop = idat$y_population[length(u)+1:length(y)],
                                                     y_county = idat$y_county[length(u)+1:length(y)],
                                                     y_period = idat$y_period[length(u)+1:length(y)],
                                                     y_id = idat$y_ID[length(u)+1:length(y)])))
stk.uy <- inla.stack(stk.u, stk.y)


ccap_shrink_more
save.image('scratch/start_model_materials.RData')
# 
# base_form  <- y ~ 0 + u.b0 + y.b0  +
#   f(u_year, model="ar1", replicate=u_id, fixed=TRUE) +
#   f(y_year, copy = 'u_year',fixed=FALSE)
# 
# #  f(u_county,model = 'iid',fixed=TRUE) + f(y_county, copy='u_county', fixed=FALSE) +
# #  f(i.u, model=spde,fixed=TRUE) + f(i.y, copy='i.u', fixed=FALSE)
# 
# full_form  <- y ~ 0 + u.b0 + y.b0 + 
#   u_cdd + y_cdd +
#   u_place + y_place + 
#   u_place:u_cdd +
#   y_place:y_cdd +
#   f(u_year, model="ar1", replicate=ID, fixed=TRUE) +
#   f(y_year, copy = 'u_year',fixed=FALSE) +
#   f(u_county,model = 'iid',fixed=TRUE) + f(y_county, copy='u_county', fixed=FALSE) +
#   f(i.y, model=spde,fixed=TRUE) + f(i.u, copy='i.y', fixed=FALSE)
# 
# res_base.uy <- inla(base_form,
#                     family=c('binomial', 'gaussian'), data=inla.stack.data(stk.uy), control.compute=list(dic=TRUE),
#                     control.predictor=list(A=inla.stack.A(stk.uy), compute=TRUE),
#                     control.fixed = list(expand.factor.strategy='inla'),
#                     control.inla= list(int.strategy = "eb",strategy = "gaussian",diagonal = 1000),verbose=T) 
# 
# res_full.uy <- inla(full_form,
#                family=c('binomial', 'gaussian'), data=inla.stack.data(stk.uy), control.compute=list(dic=TRUE),
#                control.predictor=list(A=inla.stack.A(stk.uy), compute=TRUE),
#                control.inla= list(int.strategy = "eb",strategy = "gaussian",diagonal = 1000)) 
# 
# 
# res.zy <- inla(Y ~ 0 + u.b0 + y.b0 + 
#                 u_cdd + y_cdd +
#                 u_place + y_place + 
#                 u_place:u_cdd +
#                 y_place:y_cdd +
#              #   f(u_period, group=u_ID,model="ar1") +
#               #  f(y_period,copy = 'u_period',fixed=FALSE)+
#               f(u_county,model = 'iid') +
#                  f(y_county,model = 'iid') +
#                  f(i.u, model=spde) + f(i.y, copy='i.u', fixed=FALSE),
#                family=c('binomial', 'gaussian'),num.threads=16,
#                data=inla.stack.data(stk.uy), control.compute=list(dic=TRUE),
#               control.inla= list(int.strategy = "eb",strategy = "gaussian",diagonal = 1000),
#                control.predictor=list(A=inla.stack.A(stk.uy), compute=TRUE),verbose = T)
# 
# save.image('scratch/inla_scratch.RData')
# # inla.doc("pc.prec")
# 
# 
# # 
# # 
# # library(gridExtra)
# # levelplot(prd.m, col.regions=topo.colors(99), main='latent field mean',
# #           xlab='', ylab='', scales=list(draw=FALSE))
# # levelplot(prd.s, col.regions=topo.colors(99), main='latent field mean',
# #           xlab='', ylab='', scales=list(draw=FALSE))
# # 
# # grid.arrange(levelplot(prd.m, col.regions=topo.colors(99), main='latent field mean',
# #                        xlab='', ylab='', scales=list(draw=FALSE)),
# #              levelplot(matrix(res5g$summary.fitt[igr,1], 101),
# #                        xlab='', ylab='', main='response mean',
# #                        col.regions=topo.colors(99), scales=list(draw=FALSE)),
# #              levelplot(prd0.s, col.regions=topo.colors(99), main='latent field SD',
# #                        xlab='', ylab='', scales=list(draw=FALSE)),
# #              levelplot(matrix(res5g$summary.fitt[igr,2], 101),
# #                        xlab='', ylab='', main='response SD',
# #                        col.regions=topo.colors(99), scales=list(draw=FALSE)),nrow=2)
# #              
# #              
# # res <- inla(resp ~ 0 + m + f(i,model = spde),
# #             data = inla.stack.data(stkgrid),
# #             control.predictor=list(A=inla.stack.A(stkgrid)))
# # 
# # summary(res)
# # 
# # #num.threads = 4
# # plot(ccap_duval_shrink )
# # plot(ccaps_stack_duval)
# # 
# # ccaps_stack_duval
# # ccap_duval_shrink
# # 577 * 709 * 4
# # raster::
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # 
# # stk.all <- inla.stack(stk5, stkgrid)
# # res5g <- inla(resp ~ 0 + m + f(i, model=spde5),
# #               data=inla.stack.data(stk.all),
# #               control.predictor=list(A=inla.stack.A(stk.all),
# #                                      compute=TRUE), quantiles=NULL,
# #               control.results=list(return.marginals.random=FALSE,
# #                                    return.marginals.predictor=FALSE))
# # res5g$cpu
# # 
# # 
# # 
# # 
# # 
# # dim(p)
# # ccaps_stack_duval[[1]]
# # head(p@data)
# # 
# # plot(fl_duval)
# # plot(fl_places_duval,add=T,col='red')
# # plot(cdd_duval,col='red',add=T)
# # 
# # proj4string(cdd_duval)
# # proj4string(fl_duval)
# # 
# # 
# # 
# # plot(ccaps_stack_duval)
# # plot(fl_duval)
# # 
# # ggplot(aes(y = Net_Change_In_Developed_Area,x = as.factor(ToPeriod),group = CountyName),data = dev_co) +
# #   geom_path()
# # 
# # rasterVis::magmaTheme(fl_96_dev)
# # library(tidyverse)
# # dists <- read_csv('input/florida.districts.2017 - 1996-2011.csv')
# # # Extract raster values to polygons                             
# # ( v <- extract(fl_96_dev, fl,na.rm=T) )
# # # Get class counts for each polygon
# # v.counts <- lapply(v,table)
# # # Calculate class percentages for each polygon
# # (v.pct <- lapply(v.counts, FUN=function(x){ x / sum(x) }))
# # do.call(rbind,v.pct)
# # head(developed_96 %>% filter(grepl('Miami|Manatee|Hillsb|Leon|Johns',CountyName))%>%
# #        dplyr::select(-FIPS) %>% mutate(prop = Developed_Area_SqMiles/Total_Area_SqMiles)%>%
# #        dplyr::select(CountyName,prop))m
# 
# 
# 
# 
# 
# 
# 
# #12625
# 
