
dev_function <- function(raster, functionType = 'generalized')
{
  #options:  1 - weighted area by itensity (see https://coast.noaa.gov/digitalcoast/training/ccap-land-cover-classifications.html)
  # 2- low intensity coverage only
  # 3- all developed area
  if(functionType == 'intensity')
  {devFun <- function(x) {
    return((  sum(getValues(x)  == 2,na.rm=T) * 0.9 
            + sum(getValues(x)  == 3,na.rm=T) * .65 
            + sum(getValues(x)  == 4,na.rm=T) * 0.35 
            + sum(getValues(x)  == 5,na.rm=T) * 0.1) 
           / sum(!getValues(x)  %in% 21:23,na.rm=T))}}
  
  if(functionType == 'low_intensity')
  {devFun <- function(x) {
    return(sum(getValues(x)  == 4,na.rm=T) / sum(!(getValues(x)  %in% 21:23),na.rm=T))}}
  
  if(functionType == 'generalized')
  {devFun <- function(x) {
    return(sum(getValues(x)  %in% 2:5,na.rm=T) / sum(!(getValues(x)  %in% 21:23),na.rm=T))}}
  return(devFun(raster))
}

  