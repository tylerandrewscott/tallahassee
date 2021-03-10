
sprawl_index_function <- function(raster, mask, functionType = 'generalized',small_area=TRUE)
{
  if(functionType == 'intensity')
  {develUndevelAndWater = raster
  }else if((functionType == 'generalized')||(functionType == 'inverted')){
    develUndevelAndWater <- reclassify(raster,cbind(c(2:5,21:23,0:1,6:20),
                                                    c(rep(2,length(2:5)),rep(1,length(21:23)),rep(NA,length(0:1)),rep(0,length(6:20)))))
    } else{
      # ERROR!
    }
  
if(functionType == 'intensity')
{focalFun <- function(x) {
    return((sum(x == 2,na.rm=T) * 0.9 
            + sum(x == 3,na.rm=T) * .65 
            + sum(x == 4,na.rm=T) * 0.35 
            + sum(x == 5,na.rm=T) * 0.1) 
           / sum(!(x %in% 21:23),na.rm=T))}}
if(functionType == 'generalized')
{focalFun <- function(x) { return(sum(x == 0,na.rm=T) / sum(x != 1,na.rm=T)) }}  # count undeveloped
if(functionType == 'inverted')
{focalFun <- function(x) { return(sum(x == 2,na.rm=T) / sum(x != 1,na.rm=T)) }} # count developed
  
developed <- reclassify(raster, cbind(c(c(2:5),c(0:1),c(6:30)),
                                      c(rep(1,length(2:5)),rep(NA,length(0:1)),rep(0,length(6:30)))))
developed[developed == 0] <- NA		

dryland <- reclassify(raster, cbind(c(c(21:23),c(0:1),c(2:20)),c(rep(0,length(21:23)),rep(NA,length(0:1)),rep(1,length(2:20)))))
surroundingRatio <- focal(develUndevelAndWater, w=mask, fun=focalFun)
if(!all(is.na(surroundingRatio[developed])))
{
sprawl <- surroundingRatio * developed
sprawlT <- trim(sprawl)
developedT <- crop(developed, sprawlT)
sprawlIndex <- cellStats(sprawl, stat="sum")/freq(developedT, value=1,useNA = 'no')
}
else {sprawlIndex = NA}
sprawlIndex
}








