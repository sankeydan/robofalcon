lonlat2metres = function(lon, lat){
  # lon = falc[,1]
  # lat = falc[,2]
  
  ###### LIBRARIES
  library(adehabitatHR)
  library(rgdal)
  
  ###### SCRIPT
  cb = cbind ( lon, lat)
  wh = which ( complete.cases(cb))
  cb2 = matrix(NA,nrow(cb),2)
  cb = cb[complete.cases(cb),]
  gps.sp.latlong <- SpatialPoints( cb, proj4string=CRS("+proj=longlat +ellps=WGS84"))
  gps.sp.utm <- spTransform(gps.sp.latlong, CRS())
  gps.mat.utm <- coordinates(gps.sp.utm)
  gps.mat.utm = as.data.frame(gps.mat.utm)
  cb2[wh,1] =  gps.mat.utm[,1]
  cb2[wh,2] =  gps.mat.utm[,2]
  cb2 = as.data.frame(cb2)
  names(cb2) = c("x","y")
  return(cb2)
}
