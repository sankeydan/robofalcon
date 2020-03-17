d2c = function( dat, return_centorid = F) {

  dist2c = apply( dat[,c("lon","lat"),],1,function(x){
    #x=data[1,c("lon","lat"),]
    clon = mean( x["lon",],na.rm = T)
    clat = mean( x["lat",],na.rm = T)
    return(get_dist(x[1,],x[2,],clon,clat,method= "distance"))
  })
  dist2c = t(dist2c)

  if ( !return_centorid){
  return(dist2c)
  } else{
    centroid = cbind( lon = rowMeans(dat[,"lon",],na.rm=T),
                      lat = rowMeans(dat[,"lat",],na.rm=T))
    return(centroid)
  }

}
